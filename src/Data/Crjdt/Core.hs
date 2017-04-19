{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
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
  , key :: BasicKey
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Eq tag => Ord (TaggedKey tag) where
  compare (TK _ k1) (TK _ k2) = k1 `compare` k2

data Key tag where
  Key :: BasicKey -> Key Void
  TaggedKey :: TaggedKey tag -> Key tag

data BasicKey
  = DocKey
  | Head
  | Tail
  | I Id
  | Str Text
  deriving (Show, Eq, Ord)

deriving instance Eq tag => Ord (Key tag)

instance IsString (Key Void) where
  fromString = Key . Str . fromString

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
docKey = tagWith MapT DocKey

tagWith :: tag -> BasicKey -> Key tag
tagWith t = TaggedKey . TK t

basicKey :: Key tag -> BasicKey
basicKey (Key k) = k
basicKey (TaggedKey (TK _ k)) = k

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
  { document = BranchDocument (Branch mempty mempty mempty MapT)
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

reTag :: a -> Key Void -> Key a
reTag given (Key k) = tagWith given k
reTag given (TaggedKey (TK _ k)) = tagWith given k

appendWith :: Tag -> Key Void -> Cursor -> Cursor
appendWith t k (Cursor p final) = Cursor (p `mappend` Seq.singleton (reTag t final)) k

-- Avoiding lens dependency for now
setFinalKey :: Cursor -> Key Void -> Cursor
setFinalKey c fk = c { finalKey = fk }

setPath :: Cursor -> Seq.Seq (Key Tag) -> Cursor
setPath c newpath = c { path = newpath }

type Ctx m = (MonadError EvalError m, MonadState Context m)

-- still avoiding lens, but almost there :-)
findChild :: Key Tag -> Document Tag -> Maybe (Document Tag)
findChild t (BranchDocument (Branch {branchTag = ListT, ..})) = M.lookup t children
findChild t (BranchDocument (Branch {branchTag = MapT, ..})) = M.lookup t children
findChild _ _ = Nothing

childGet :: Key Tag -> Document Tag -> Document Tag
childGet k = fromMaybe (choose (getTag k)) . findChild k
  where choose MapT = BranchDocument $ Branch mempty mempty mempty MapT
        choose ListT = BranchDocument $ Branch mempty mempty (M.singleton Head Tail) ListT
        choose RegT = LeafDocument mempty

lookupCtx :: Var -> Context -> Maybe Cursor
lookupCtx v = M.lookup v . variables

getPresence :: Key Void -> Document Tag -> Set Id
getPresence k (BranchDocument (Branch {branchTag = ListT, ..})) = fromMaybe mempty (M.lookup k presence)
getPresence _ _ = mempty

next :: Key Void -> Document Tag -> BasicKey
next (Key key) (BranchDocument (Branch {branchTag = ListT, ..})) = fromMaybe Tail $
  M.lookup key keyOrder
next _ _ = Tail

data Id = Id
  { sequenceNumber :: Integer
  , replicaNumber :: Integer
  } deriving (Show, Eq, Ord)

data Branch tag = Branch
  { children :: Map (Key tag) (Document tag)
  , presence :: Map (Key Void) (Set Id)
  , keyOrder :: Map BasicKey BasicKey
  , branchTag :: tag
  }

updatePresence :: Key Void -> Set Id -> Document tag -> Document tag
updatePresence key (Set.null -> True) (BranchDocument b) = BranchDocument b
  { presence = M.delete key (presence b) }
updatePresence key newPresence (BranchDocument b) = BranchDocument b
  { presence = M.insert key newPresence $ presence b }
updatePresence _ _ d = d

data RegDocument = RegDocument { values :: M.Map Id Val }

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

addVariable :: Ctx m => Var -> Cursor -> m ()
addVariable v cur = modify $ \c -> c { variables = M.insert v cur (variables c)}
{-# INLINE addVariable #-}

-- FIXME: State (Document Tag) (Set Id)
clearElem :: Set Id -> Key Void -> State (Document Tag) (Set Id)
clearElem deps key = do
  presence <- clearAny deps key
  presence' <- getPresence key <$> get
  let newPresence = presence `mappend` presence' Set.\\ deps
  modify (updatePresence key newPresence)
  pure (newPresence)

clearAny :: Set Id -> Key Void -> State (Document Tag) (Set Id)
clearAny deps key = mconcat <$> traverse clearAll [MapT, ListT, RegT]
  where clearAll t = clear deps (reTag t key)

clear :: Set Id -> Key Tag -> State (Document Tag) (Set Id)
clear deps key = get >>= (clear' <*> findChild key)
  where
    {-# INLINE clear' #-}
    clear' _ Nothing = pure mempty
    clear' _ (Just (LeafDocument reg)) = put (LeafDocument $ RegDocument c) *> pure (M.keysSet c)
      where c = M.filterWithKey (\k _ -> k `Set.notMember` deps) $ values reg
    clear' d (Just child@(BranchDocument (Branch  {branchTag = MapT}))) = clearBranch d $ clearMap child
    clear' d (Just child@(BranchDocument (Branch {branchTag = ListT}))) = clearBranch d $ clearList child

    {-# INLINE clearBranch #-}
    clearBranch d clearWhich = do
      presence <- clearWhich deps
      modify (\d' -> addChild key d' d)
      pure presence

addChild :: Key Tag -> Document Tag -> Document Tag -> Document Tag
addChild key _ d@(LeafDocument _) = d
addChild key child (BranchDocument d) = BranchDocument d { children = M.insert key child (children d)}

clearMap, clearList :: Document Tag -> Set Id -> State (Document Tag) (Set Id)
clearMap child deps = put child *> clearMap' mempty
  where clearMap' acc = do
          mapKeys <- allKeys <$> get
          case Set.toList (mapKeys Set.\\ acc) of
            [] -> pure mempty
            (k: _) -> do
              p1 <- clearElem deps (unTag k)
              p2 <- clearMap' (Set.insert k acc)
              pure (p1 `mappend` p2)
        allKeys (BranchDocument (Branch {branchTag = MapT, ..})) = keysSet children
        allKeys _ = mempty

clearList child deps = put child *> clearList' Head
  where clearList' Tail = pure mempty
        clearList' hasMore = do
          nextt <- next (Key hasMore) <$> get
          p1 <- clearElem deps (Key nextt)
          p2 <- clearList' nextt
          pure (p1 `mappend` p2)


addId :: Mutation -> Key Tag -> Id -> Document Tag -> Document Tag
addId DeleteMutation _ _ d = d
addId _ t i (BranchDocument b) = BranchDocument b
  { presence = M.alter (maybe (Just $ Set.singleton i) (Just . Set.insert i)) (unTag t) $ presence b }

applyOp :: Operation -> Document Tag -> Document Tag
applyOp op d = case (path $ opCur op) of
  (viewl -> EmptyL) -> case op of
    Operation {opMutation = AssignMutation val, ..} ->
      _x

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
  let nextKey = Key $ getNextKey (document ctx)
  case (nextKey /= tailKey, Set.null (getPresence nextKey (document ctx))) of
    (True, True) -> pure (Cursor mempty nextKey)
    (True, False) -> stepNext d (Cursor mempty nextKey)
    (False, _) -> pure c
stepNext d c@(Cursor (viewl -> (x :< xs)) _) = maybe (pure c) f (findChild x d)
  where f = fmap (setFinalKey c . finalKey) . (`stepNext` (setPath c xs))
stepNext _ c = pure c

eval :: Ctx m => Expr -> m Result
eval Doc = pure $ Cursor Seq.empty $ unTag docKey
eval (GetKey expr k) = do
  cursor <- eval expr
  case finalKey cursor of
    (Key Head) -> throwError GetOnHead
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
