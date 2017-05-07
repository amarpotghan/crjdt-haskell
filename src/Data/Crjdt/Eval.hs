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
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Data.Foldable (traverse_)
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Free (iterM)

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
  (LeafDocument l) -> M.elems (registers l)
  _ -> mempty

keysOf :: Ctx m => Expr -> m (Set.Set (Key Void))
keysOf e = partsOf e MapT $ \case
  (BranchDocument (Branch{branchTag = MapT,..})) -> M.keysSet $ M.filter (not . Set.null) presence
  _ -> mempty

partsOf :: (Ctx m, Monoid a) => Expr -> Tag -> (Document Tag -> a) -> m a
partsOf e tag f = eval e >>= \c -> partsOf' c f . document <$> get
  where
    partsOf' :: Monoid m => Cursor -> (Document Tag -> m) -> Document Tag -> m
    partsOf' Cursor{..} getParts d = fromMaybe mempty $ case viewl path of
      EmptyL -> getParts <$> findChild (tagWith tag $ basicKey finalKey) d
      (x :< xs) -> partsOf' (Cursor xs finalKey) getParts <$> findChild x d

addVariable :: Ctx m => Var -> Cursor -> m ()
addVariable v cur = modify $ \c -> c { variables = M.insert v cur (variables c)}
{-# INLINE addVariable #-}

applyRemote :: Ctx m => m ()
applyRemote = get >>= \c ->
  let alreadyProcessed op cc = opId op `Set.member` history cc
      satisfiesDeps op cc = opDeps op `Set.isSubsetOf` history cc
      applyRemote' op = do
        cc <- get
        when (not (alreadyProcessed op cc) && satisfiesDeps op cc) $ put cc
          { replicaGlobal = replicaGlobal cc `max` (sequenceNumber . opId $ op)
          , document = applyOp op (document cc)
          , history = Set.insert (opId op) (history cc)
          }
  in traverse_ applyRemote' (received c)
{-# INLINE applyRemote #-}

applyLocal :: Ctx m => Mutation -> Cursor -> m ()
applyLocal mut cur = modify $ \c ->
  let op = Operation
        { opId = mkId (replicaGlobal c + 1) (replicaId c)
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

eval :: Ctx m => Expr -> m Result
eval Doc = pure $ Cursor Seq.empty $ unTag docKey
eval (GetKey expr k) = do
  cursor <- eval expr
  case finalKey cursor of
    (Key Head) -> throwError GetOnHead
    _ -> pure (appendWith MapT k cursor)
eval (Var var) = get >>= maybe (throwError (UndefinedVariable var)) pure . lookupCtx var
eval (Iter expr) = appendWith ListT (Key Head) <$> eval expr
eval (Next expr) = get >>= \(document -> d) -> stepNext d <$> eval expr

execCmd :: Ctx m => Cmd (m a) -> m a
execCmd (Let x expr c) = (eval expr >>= \cur -> addVariable (Variable x) cur *> pure (Var $ Variable x)) >>= c
execCmd (Assign expr v c) = (eval expr >>= applyLocal (AssignMutation v)) >> c
execCmd (InsertAfter expr v c) = (eval expr >>= applyLocal (InsertMutation v)) >> c
execCmd (Delete expr c) = (eval expr >>= applyLocal DeleteMutation) >> c
execCmd (Yield c) = applyRemote >> c
execCmd (Values expr c) = valuesOf expr >>= c
execCmd (Keys expr c) = keysOf expr >>= c

execute :: Ctx m => Command a -> m a
execute = iterM execCmd . runCommand
