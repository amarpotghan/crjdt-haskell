{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Crjdt.Eval
  ( Eval(..)
  , EvalError(..)
  , keysOf
  , valuesOf
  , run
  , evalEval
  , execEval
  , addVariable
  , execute
  , eval
  ) where

import Data.Void
import Data.Sequence (ViewL(..), viewl)
import qualified Data.Sequence as Seq
import Data.Map as M
import Data.Set as Set
import Data.Foldable (traverse_)
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Except

import Data.Crjdt.Types
import Data.Crjdt.Context
import Data.Crjdt.Internal.Core

data EvalError
  = GetOnHead
  | UndefinedVariable Var
  deriving (Show, Eq)

newtype Eval a = Eval
  { runEval :: ExceptT EvalError (State Context) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadFix
  , MonadError EvalError
  , MonadState Context
  )
type Result = Cursor
  -- = Ks [Key Void]
  -- \| Vs [Val]
  -- \| Mark Cursor
  -- deriving (Show, Eq)

initial :: ReplicaId -> Context
initial rid = Context
  { document = BranchDocument (Branch mempty mempty mempty MapT)
  , replicaGlobal = 0
  , variables = mempty
  , replicaId = rid
  , queue = mempty
  , history = mempty
  , received = mempty
  }

type Ctx m = (MonadError EvalError m, MonadState Context m)

run :: ReplicaId -> Eval a -> (Either EvalError a, Context)
run rid = (`runState` (initial rid)) . runExceptT . runEval

evalEval :: ReplicaId -> Expr -> Either EvalError Cursor
evalEval rid = (`evalState` (initial rid)) . runExceptT . runEval . eval

execEval :: ReplicaId -> Eval a -> Context
execEval rid = (`execState` (initial rid)) . runExceptT . runEval

valuesOf :: Ctx m => Expr -> m [Val]
valuesOf e = partsOf e RegT $ \case
  (LeafDocument l) -> M.elems (values l)
  _ -> mempty

keysOf :: Ctx m => Expr -> m (Set.Set (Key Void))
keysOf e = partsOf e MapT $ \case
  (BranchDocument (Branch{branchTag = MapT,..})) -> M.keysSet $ M.filter (not . Set.null) presence
  _ -> mempty

partsOf :: (Ctx m, Monoid a) => Expr -> Tag -> (Document Tag -> a) -> m a
partsOf e tag f = eval e >>= \c -> partsOf' c f . document <$> get
  where
    partsOf' :: Monoid m => Cursor -> (Document Tag -> m) -> Document Tag -> m
    partsOf' Cursor{..} getParts d = case viewl path of
      EmptyL -> maybe mempty getParts $ findChild (tagWith tag $ basicKey finalKey) d
      (x :< xs) -> maybe mempty (partsOf' (Cursor xs finalKey) getParts) $ findChild x d

addVariable :: Ctx m => Var -> Cursor -> m ()
addVariable v cur = modify $ \c -> c { variables = M.insert v cur (variables c)}
{-# INLINE addVariable #-}

applyRemote :: Ctx m => m ()
applyRemote = get >>= \c ->
  let alreadyProcessed op = opId op `Set.member` history c
      satisfiesDeps op = opDeps op `Set.isSubsetOf` history c
      applyRemote' op = put c
        { replicaGlobal = replicaGlobal c `max` (sequenceNumber . opId $ op)
        , document = applyOp op (document c)
        , history = Set.insert (opId op) (history c)
        }
  in traverse_ applyRemote' $ Seq.filter (liftM2 (&&) alreadyProcessed satisfiesDeps) (received c)
{-# INLINE applyRemote #-}

applyLocal :: Ctx m => Mutation -> Cursor -> m ()
applyLocal mut cur = modify $ \c ->
  let op = Operation
        { opId = Id (replicaGlobal c + 1) (replicaId c)
        , opDeps = history c
        , opCur = cur
        , opMutation = mut
        }
  in c { document = applyOp op (document c)
       , replicaGlobal = replicaGlobal c + 1
       , history = Set.insert (opId op) (history c)
       , queue = queue c Seq.|> op
       }
{-# INLINE applyLocal #-}

stepNext :: Ctx m => Document Tag -> Cursor -> m Cursor
stepNext d c@(Cursor (viewl -> Seq.EmptyL) (next -> getNextKey)) = get >>= \ctx -> do
  let nextKey = getNextKey (document ctx)
  case (nextKey /= Key Tail, Set.null (getPresence nextKey (document ctx))) of
    (True, True) -> pure (Cursor mempty nextKey)
    (True, False) -> stepNext d (Cursor mempty nextKey)
    (False, _) -> pure c
stepNext d c@(Cursor (viewl -> (x :< xs)) _) = maybe (pure c) f (findChild x d)
  where f = fmap ((`setFinalKey` c) . finalKey) . (`stepNext` (setPath xs c))
stepNext _ c = pure c

eval :: Ctx m => Expr -> m Result
eval Doc = pure $ Cursor Seq.empty $ unTag docKey
eval (GetKey expr k) = do
  cursor <- eval expr
  case finalKey cursor of
    (Key Head) -> throwError GetOnHead
    _ -> pure (appendWith MapT k cursor)
eval (Var var) = get >>= maybe (throwError (UndefinedVariable var)) pure . lookupCtx var
eval (Iter expr) = appendWith ListT (Key Head) <$> eval expr
eval (Next expr) = get >>= \(document -> d) -> eval expr >>= stepNext d

execute :: Ctx m => Cmd -> m ()
execute (Let x expr) = eval expr >>= addVariable (Variable x)
execute (Assign expr value) = eval expr >>= applyLocal (AssignMutation value)
execute (InsertAfter expr value) = eval expr >>= applyLocal (InsertMutation value)
execute (Delete expr) = eval expr >>= applyLocal DeleteMutation
execute Yield = applyRemote
execute (cmd1 :> cmd2) = execute cmd1 *> execute cmd2
