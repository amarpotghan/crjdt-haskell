{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Crjdt.Core where

import Data.Text
import Data.String
import Data.Void
import Data.Maybe
import Data.Sequence (ViewL(..), viewl)
import qualified Data.Sequence as Seq
import Data.Map as M
import Data.Set as Set
import Data.Foldable (traverse_)
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Except

newtype Var = Variable { getName :: Text } deriving (Show, Eq, Ord)

instance IsString Var where
  fromString = Variable . fromString

data TaggedKey tag = TK
  { tag :: tag
  , key :: Text
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Eq tag => Ord (TaggedKey tag) where
  compare (TK _ k1) (TK _ k2) = k1 `compare` k2

data Key tag where
  Key :: Text -> Key Void
  TaggedKey :: TaggedKey tag -> Key tag

instance Eq tag => Ord (Key tag) where
  compare (Key k1) (Key k2) = k1 `compare` k2
  compare (TaggedKey k1) (TaggedKey k2) = k1 `compare` k2
  compare (TaggedKey (TK _ k1)) (Key k2) = k1 `compare` k2
  compare (Key k1) (TaggedKey (TK _ k2)) = k1 `compare` k2

instance IsString (Key Void) where
  fromString = Key . fromString

instance Show tag => Show (Key tag) where
  show (Key t) = show t
  show (TaggedKey taggedKey) = show taggedKey

instance Eq tag => Eq (Key tag) where
  (Key t) == (Key t1) = t == t1
  (TaggedKey t) == (TaggedKey t1) = t == t1
  (Key _) == (TaggedKey _) = False
  (TaggedKey _) == (Key _) = False

data Tag
  = MapT
  | ListT
  | RegT
  deriving (Show, Eq, Ord)

data Val
  = Number Int
  | StringLit Text
  | BoolLit Bool
  | Null
  | EmptyObject
  | EmptyArray
  deriving (Show, Eq)

data Cmd
  = Let Text Expr
  | Assign Expr Val
  | InsertAfter Expr Val
  | Delete Expr
  | Yield
  | Cmd :> Cmd
  deriving (Show, Eq)

data Expr
  = Doc
  | Var Var
  | Keys Expr
  | Values Expr
  | Iter Expr
  | Next Expr
  | GetKey Expr (Key Void)
  deriving (Show, Eq)

type Result = Cursor
  -- = Ks [Key Void]
  -- \| Vs [Val]
  -- \| Mark Cursor
  -- deriving (Show, Eq)

-- DOC
docKey :: Key Tag
docKey = tagWith MapT "doc"

tagWith :: tag -> Text -> Key tag
tagWith t = TaggedKey . TK t

data Cursor = Cursor
  { path :: Seq.Seq (Key Tag)
  , finalKey :: Key Void
  } deriving (Show, Eq)

type ReplicaId = Integer
type GlobalReplicaCounter = Integer

data Context = Context
  { document :: Document Tag
  , variables :: M.Map Var Cursor
  , replicaId :: ReplicaId
  , replicaGlobal :: GlobalReplicaCounter
  , history :: Set Id
  , queue :: Seq.Seq Operation
  , received :: Seq.Seq Operation
  }

data EvalError
  = GetOnHead
  | UndefinedVariable Var

headKey, tailKey :: Key Void
headKey = "head"
tailKey = "tail"

newtype Eval a
  = Eval { runEval :: ExceptT EvalError (State Context) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadFix
           , MonadError EvalError
           , MonadState Context
           )

initial :: ReplicaId -> Context
initial rid = Context
  { document = BranchDocument (Branch mempty mempty MapT)
  , replicaGlobal = 0
  , variables = mempty
  , replicaId = rid
  , queue = mempty
  , history = mempty
  , received = mempty
  }

evalEval :: ReplicaId -> Expr -> Either EvalError Cursor
evalEval rid = (`evalState` (initial rid)) . runExceptT . runEval . eval

unTag :: Key tag -> Key Void
unTag (Key k) = Key k
unTag (TaggedKey (TK _ k)) = Key k

doTag :: a -> Key Void -> Key a
doTag given (Key k) = tagWith given k
doTag given (TaggedKey (TK _ k)) = tagWith given k

appendWith :: Tag -> Key Void -> Cursor -> Cursor
appendWith t k (Cursor p final) = Cursor (p `mappend` Seq.singleton (doTag t final)) k

-- Avoiding lens dependency for now
setFinalKey :: Cursor -> Key Void -> Cursor
setFinalKey c fk = c { finalKey = fk }

setPath :: Cursor -> Seq.Seq (Key Tag) -> Cursor
setPath c newpath = c { path = newpath }

type Ctx m = (MonadError EvalError m, MonadState Context m)

-- still avoiding lens, but almost there :-)
findChild :: Tag -> Document Tag -> Maybe (Document Tag)
findChild t (BranchDocument (Branch c _ ListT)) = M.lookup t c
findChild _ _ = Nothing

lookupCtx :: Var -> Context -> Maybe Cursor
lookupCtx v = M.lookup v . variables

getPresence :: Key Void -> Document Tag -> Set Id
getPresence k (BranchDocument (Branch _ ps ListT)) = fromMaybe mempty (M.lookup k ps)
getPresence _ _ = mempty

next :: Key Void -> Document Tag -> Key Void
next _ (BranchDocument (Branch _ _ ListT)) = tailKey
next _ _ = tailKey

data Id = Id
  { sequenceNumber :: Integer
  , replicaNumber :: Integer
  } deriving (Show, Eq, Ord)

data Branch tag = Branch
  { children :: Map Tag (Document tag)
  , presence :: Map (Key Void) (Set Id)
  , branchTag :: tag
  } deriving (Functor, Foldable, Traversable)

data RegDocument

getTag :: Key Tag -> Tag
getTag (TaggedKey (TK t _)) = t

data Mutation
  = InsertMutation Val
  | DeleteMutation
  | AssignMutation Val
  deriving (Show, Eq)

data Operation = Operation
  { opId :: Id
  , opDeps :: Set.Set Id -- TODO:  version-vectors or dotted-version-vectors
  , opCur :: Cursor
  , opMutation :: Mutation
  } deriving (Eq, Show)

data Document tag
  = BranchDocument (Branch tag)
  | LeafDocument RegDocument
  deriving (Functor, Foldable, Traversable)

addVariable :: Ctx m => Var -> Cursor -> m ()
addVariable v cur = modify $ \c -> c { variables = M.insert v cur (variables c)}

applyOp :: Operation -> Document Tag -> Document Tag
applyOp _ = error "Not yet implemented"

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

stepNext :: Ctx m => Document Tag -> Cursor -> m Cursor
stepNext d c@(Cursor (viewl -> Seq.EmptyL) (next -> getNextKey)) = get >>= \ctx -> do
  let nextKey = getNextKey (document ctx)
  case (nextKey /= tailKey, Set.null (getPresence nextKey (document ctx))) of
    (True, True) -> pure (Cursor mempty nextKey)
    (True, False) -> stepNext d (Cursor mempty nextKey)
    (False, _) -> pure c
stepNext d c@(Cursor (viewl -> (x :< xs)) _) = maybe (pure c) f (findChild (getTag x) d)
  where f = fmap (setFinalKey c . finalKey) . (`stepNext` (setPath c xs))
stepNext _ c = pure c

eval :: Ctx m => Expr -> m Result
eval Doc = pure $ Cursor Seq.empty $ unTag docKey
eval (GetKey expr k) = do
  cursor <- eval expr
  case finalKey cursor of
    (Key "head") -> throwError GetOnHead
    _ -> pure (appendWith MapT k cursor)
eval (Var var) = get >>= maybe (throwError (UndefinedVariable var)) pure . lookupCtx var
eval (Iter expr) = appendWith ListT headKey <$> eval expr
eval (Next expr) = get >>= \(document -> d) -> eval expr >>= stepNext d
-- TODO: query part. Not yet implemented
eval values = eval values

execute :: Ctx m => Cmd -> m ()
execute (Let x expr) = eval expr >>= addVariable (Variable x)
execute (Assign expr value) = eval expr >>= applyLocal (AssignMutation value)
execute (InsertAfter expr value) = eval expr >>= applyLocal (InsertMutation value)
execute (Delete expr) = eval expr >>= applyLocal DeleteMutation
execute Yield = applyRemote
execute (cmd1 :> cmd2) = execute cmd1 *> execute cmd2
