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

module Data.Crjdt.Context where

import Data.Void
import Data.Maybe
import Data.Sequence (ViewL(..), viewl)
import qualified Data.Sequence as Seq
import Data.Map as M
import Data.Set as Set
import Test.SmallCheck.Series
import Control.Monad.State

import Data.Crjdt.Types
import Data.Crjdt.Internal.Core

data Tag
  = MapT
  | ListT
  | RegT
  deriving (Show, Eq, Ord)

instance Monad m => Serial m Tag where
  series = cons0 MapT \/ cons0 ListT \/ cons0 RegT

data Context = Context
  { document :: Document Tag
  , variables :: M.Map Var Cursor
  , replicaId :: ReplicaId
  , replicaGlobal :: GlobalReplicaCounter
  , history :: Set Id
  , queue :: Seq.Seq Operation
  , received :: Seq.Seq Operation
  } deriving Show

data Cursor = Cursor
  { path :: Seq.Seq (Key Tag)
  , finalKey :: Key Void
  } deriving (Show, Eq)

docKey :: Key Tag
docKey = tagWith MapT DocKey

appendWith :: Tag -> Key Void -> Cursor -> Cursor
appendWith t k (Cursor p final) = Cursor (p `mappend` Seq.singleton (reTag t final)) k

-- Avoiding lens dependency for now
setFinalKey :: Key Void -> Cursor -> Cursor
setFinalKey fk c = c { finalKey = fk }

setPath :: Seq.Seq (Key Tag) -> Cursor -> Cursor
setPath newpath c = c { path = newpath }

-- still avoiding lens, but almost there :-)
findChild :: Key Tag -> Document Tag -> Maybe (Document Tag)
findChild t (BranchDocument (Branch {branchTag = ListT, ..})) = M.lookup t children
findChild t (BranchDocument (Branch {branchTag = MapT, ..})) = M.lookup t children
findChild _ _ = Nothing

childGet :: Key Tag -> Document Tag -> Document Tag
childGet k = fromMaybe (choose (getTag k)) . findChild k
  where
    choose MapT = BranchDocument $ Branch mempty mempty mempty MapT
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

updatePresence :: Key Void -> Set Id -> Document tag -> Document tag
updatePresence key (Set.null -> True) (BranchDocument b) = BranchDocument b
  { presence = M.delete key (presence b) }
updatePresence key newPresence (BranchDocument b) = BranchDocument b
  { presence = M.insert key newPresence $ presence b }
updatePresence _ _ d = d

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

newtype RegDocument = RegDocument { values :: M.Map Id Val } deriving (Show, Eq, Monoid)

data Branch tag = Branch
  { children :: Map (Key tag) (Document tag)
  , presence :: Map (Key Void) (Set Id)
  , keyOrder :: Map BasicKey BasicKey
  , branchTag :: tag
  } deriving Show

data Document tag
  = BranchDocument (Branch tag)
  | LeafDocument RegDocument
  deriving Show

clearElem :: Set Id -> Key Void -> State (Document Tag) (Set Id)
clearElem deps key = do
  presence <- clearAny deps key
  presence' <- getPresence key <$> get
  let newPresence = presence `mappend` presence' Set.\\ deps
  modify (updatePresence key newPresence)
  pure (newPresence)

clearAny :: Set Id -> Key Void -> State (Document Tag) (Set Id)
clearAny deps key = mconcat <$> traverse clearAll [MapT, ListT, RegT]
  where
    clearAll t = clear deps (reTag t key)

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
  where
    clearMap' acc = do
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
  where
    clearList' (Key Tail) = pure mempty
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
