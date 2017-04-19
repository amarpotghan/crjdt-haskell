{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import GHC.Generics
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Except
import Test.SmallCheck
import Test.SmallCheck.Series

newtype Var = Variable { getName :: Text } deriving (Show, Eq, Ord)

instance Monad m => Serial m Var where
  series = newtypeCons (Variable .  pack)

instance IsString Var where
  fromString = Variable . fromString

data TaggedKey tag = TK
  { tag :: !tag
  , key :: !BasicKey
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Serial m tag => Serial m (TaggedKey tag) where
  series = cons2 TK

instance Eq tag => Ord (TaggedKey tag) where
  compare (TK _ k1) (TK _ k2) = k1 `compare` k2

-- TODO: rethink about this type
data Key tag where
  Key :: BasicKey -> Key Void
  TaggedKey :: TaggedKey tag -> Key tag

instance Monad m => Serial m (Key Void) where
  series = Key <$> series

data BasicKey
  = DocKey
  | Head
  | Tail
  | I Id
  | Str Text
  deriving (Show, Eq, Ord)

instance Monad m => Serial m BasicKey where
  series = cons0 DocKey \/ cons0 Head \/ cons0 Tail \/ cons1 I \/ cons1 (Str . pack)

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

instance Monad m => Serial m Tag where
  series = cons0 MapT \/ cons0 ListT \/ cons0 RegT

data Val
  = Number Int
  | StringLit Text
  | BoolLit Bool
  | Null
  | EmptyObject
  | EmptyArray
  deriving (Show, Eq)

instance Monad m => Serial m Val where
  series =
    cons1 Number \/
    (StringLit . pack <$> series) \/
    cons1 BoolLit \/
    cons0 Null \/
    cons0 EmptyObject \/
    cons0 EmptyArray

data Cmd
  = Let !Text !Expr
  | Assign !Expr !Val
  | InsertAfter !Expr !Val
  | Delete !Expr
  | Yield
  | !Cmd :> !Cmd
  deriving (Show, Eq)

data Expr
  = Doc
  | Var !Var
  -- | Keys Expr
  -- | Values Expr
  | Iter !Expr
  | Next !Expr
  | GetKey !Expr !(Key Void)
  deriving (Show, Eq, Generic)

instance Monad m => Serial m Expr where
  series =
    cons1 Var \/
    cons0 Doc \/
    cons1 Iter \/
    cons1 Next \/
    cons2 GetKey

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
  deriving (Show, Eq)

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
setFinalKey :: Key Void -> Cursor -> Cursor
setFinalKey fk c = c { finalKey = fk }

setPath :: Seq.Seq (Key Tag) -> Cursor -> Cursor
setPath newpath c = c { path = newpath }

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

next :: Key Void -> Document Tag -> Key Void
next (Key key) (BranchDocument (Branch {branchTag = ListT, ..})) = Key $ fromMaybe Tail $
  M.lookup key keyOrder
next _ _ = Key Tail

data Id = Id
  { sequenceNumber :: Integer
  , replicaNumber :: Integer
  } deriving (Show, Eq, Ord)

instance Monad m => Serial m Id where
  series = cons2 Id

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

newtype RegDocument = RegDocument { values :: M.Map Id Val } deriving (Monoid)

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
    clear' _ Nothing = pure mempty
    clear' _ (Just (LeafDocument reg)) = put (LeafDocument $ RegDocument c) *> pure (M.keysSet c)
      where c = M.filterWithKey (\k _ -> k `Set.notMember` deps) $ values reg
    clear' d (Just child@(BranchDocument (Branch  {branchTag = MapT}))) = clearBranch d $ clearMap child
    clear' d (Just child@(BranchDocument (Branch {branchTag = ListT}))) = clearBranch d $ clearList child
    clear' _ _ = pure mempty -- this should never happen. TODO: Capture this in type of Document.
    {-# INLINE clear' #-}

    clearBranch d clearWhich = do
      presence <- clearWhich deps
      modify (\d' -> addChild key d' d)
      pure presence
    {-# INLINE clearBranch #-}

addChild :: Key Tag -> Document Tag -> Document Tag -> Document Tag
addChild _ _ d@(LeafDocument _) = d
addChild key child (BranchDocument d) = BranchDocument d { children = M.insert key child (children d)}

clearMap, clearList :: Document Tag -> Set Id -> State (Document Tag) (Set Id)
clearMap child deps = put child *> clearMap' mempty
  where clearMap' acc = do
          ms <- allKeys <$> get
          case Set.toList (ms Set.\\ acc) of
            [] -> pure mempty
            (k: _) -> do
              p1 <- clearElem deps (unTag k)
              p2 <- clearMap' (Set.insert k acc)
              pure (p1 `mappend` p2)
        allKeys (BranchDocument (Branch {branchTag = MapT, ..})) = keysSet children
        allKeys _ = mempty

clearList child deps = put child *> clearList' (Key Head)
  where clearList' (Key Tail) = pure mempty
        clearList' hasMore = do
          nextt <- next hasMore <$> get
          p1 <- clearElem deps nextt
          p2 <- clearList' nextt
          pure (p1 `mappend` p2)

addId :: Mutation -> Key Tag -> Id -> Document Tag -> Document Tag
addId _ t i (BranchDocument b) = BranchDocument b
  { presence = M.alter (maybe (Just $ Set.singleton i) (Just . Set.insert i)) (unTag t) $ presence b }
addId DeleteMutation _ _ d = d
addId _ _ _ d = d

applyOp :: Operation -> Document Tag -> Document Tag
applyOp o@Operation{..} d = case viewl (path opCur) of
  EmptyL -> case opMutation of
    AssignMutation val -> case val of
      EmptyObject -> assignBranch MapT d
      EmptyArray -> assignBranch ListT d
      other -> assignLeaf other d
      where
        assignBranch tag = execState $ do
          let key@(Key k) = finalKey opCur
              tagged = tagWith tag k
          _ <- clearElem opDeps key
          modify $ addId opMutation tagged opId
          child <- childGet tagged <$> get
          modify (addChild tagged child)
        assignLeaf other = execState $ do
          let tagged = tagWith RegT (basicKey $ finalKey opCur)
          _ <- clear opDeps tagged
          modify $ addId opMutation tagged opId
          child <- childGet tagged <$> get
          case child of
            LeafDocument (RegDocument vals) ->
              modify (addChild tagged $ LeafDocument $ RegDocument (M.insert opId other vals))
            branchChild -> modify (addChild tagged branchChild)

    InsertMutation val -> insert' nextKey
      where
        key = finalKey opCur
        nextKey = next key d
        assign =
          let newDoc = applyOp o
                { opCur = setPath mempty . setFinalKey (finalKey opCur) $ opCur
                , opMutation = AssignMutation val
                } d
          in case newDoc of
               BranchDocument b -> BranchDocument $ b
                 { keyOrder = M.insert (I opId) (basicKey nextKey) . M.insert (basicKey key) (I opId) $ keyOrder b}
               nd -> nd
        insert' (Key (I kid)) | opId < kid = assign
        insert' (Key Tail) = assign
        insert' (Key (I kid))
          | opId > kid = applyOp o
            { opCur = setPath mempty . setFinalKey (finalKey opCur)$  opCur } d
        insert' _ = d
    DeleteMutation -> execState (clearElem opDeps (finalKey opCur)) d

  (x :< xs) ->
    let c = childGet x d
        child = applyOp (o {opCur = setPath xs opCur}) c
        newDoc = addId opMutation x opId d
    in addChild x child newDoc

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
