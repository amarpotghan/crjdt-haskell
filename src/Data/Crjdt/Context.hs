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

module Data.Crjdt.Context
  ( Context(..)
  , Cursor(..)
  , Operation(..)
  , Tag(..)
  , Mutation(..)
  , Document(..)
  , Branch(..)
  , RegDocument(..)
  , findChild
  , applyOp
  , getPresence
  , lookupCtx
  , appendWith
  , docKey
  , stepNext
  ) where

import Data.Void
import Data.Foldable as Foldable (toList)
import Data.Maybe
import Data.Sequence (ViewL(..), viewl)
import qualified Data.Sequence as Seq
import Data.Map as M
import Data.Set as Set
import Control.Monad.State

import Data.Crjdt.Types
import Data.Crjdt.Internal.Core

data Tag
  = MapT
  | ListT
  | RegT
  deriving (Show, Eq, Ord)

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
findChild t = ((M.lookup t . children) =<<) . branchOf

childGet :: Key Tag -> Document Tag -> Document Tag
childGet k = fromMaybe (choose (getTag k)) . findChild k
  where
    choose MapT = BranchDocument $ Branch mempty mempty mempty MapT
    choose ListT = BranchDocument $ Branch mempty mempty (M.singleton Head Tail) ListT
    choose RegT = LeafDocument mempty

lookupCtx :: Var -> Context -> Maybe Cursor
lookupCtx v = M.lookup v . variables

getPresence :: Key Void -> Document Tag -> Set Id
getPresence k =  fromMaybe mempty . ((M.lookup k . presence) =<<) . branchOf

next :: Key Void -> Document Tag -> Key Void
next (Key key) d = Key $ fromMaybe Tail $ next'
  where
    next' = branchOf d >>= ensureList >>= M.lookup key . keyOrder
    ensureList b@Branch{branchTag = ListT} = Just b
    ensureList _ = Nothing
next k _ = k

updatePresence :: Key Void -> Set Id -> Document tag -> Document tag
updatePresence key deps d = fromMaybe d . fmap (BranchDocument . newBranch) . branchOf $ d
  where newBranch b =
          if (Set.null deps)
          then b { presence = M.delete key (presence b) }
          else b { presence = M.insert key deps $ presence b }

branchOf :: Document tag -> Maybe (Branch tag)
branchOf (LeafDocument _) = Nothing
branchOf (BranchDocument b) = Just b

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

newtype RegDocument = RegDocument { registers :: M.Map Id Val } deriving (Show, Eq, Monoid)

data Branch tag = Branch
  { children :: Map (Key tag) (Document tag)
  , presence :: Map (Key Void) (Set Id)
  , keyOrder :: Map BasicKey BasicKey
  , branchTag :: tag
  } deriving (Eq, Show)

data Document tag
  = BranchDocument (Branch tag)
  | LeafDocument RegDocument
  deriving (Eq, Show)

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
    clear' d (Just (LeafDocument reg)) = put (addChild key (LeafDocument $ RegDocument c) d) *> pure (M.keysSet c)
      where c = M.filterWithKey (\k _ -> k `Set.notMember` deps) $ registers reg
    clear' d (Just b)  = fromMaybe (pure mempty) (fmap (clearBranch d) (branchOf b >>= (`chooseClear` b) . branchTag))

    clearBranch d clearWhich = do
      presence <- clearWhich deps
      modify (\d' -> addChild key d' d)
      pure presence

    chooseClear ListT = Just . clearList
    chooseClear MapT = Just . clearMap
    chooseClear RegT = const Nothing

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
          p1 <- clearElem deps hasMore
          p2 <- clearList' nextt
          pure (p1 `mappend` p2)

stepNext :: Document Tag -> Cursor -> Cursor
stepNext d c@(Cursor (viewl -> Seq.EmptyL) (next -> getNextKey)) =
  let nextKey = getNextKey d
      newCur = Cursor mempty nextKey
  in case (nextKey /= Key Tail, Set.null (getPresence nextKey d)) of
    (True, True) -> stepNext d newCur
    (True, False) -> newCur
    (False, _) -> c

stepNext d c@(Cursor (viewl -> (x :< xs)) _) = maybe c f (findChild x d)
  where f nd = setFinalKey (finalKey $ stepNext nd (setPath xs c)) c
stepNext _ c = c

addChild :: Key Tag -> Document Tag -> Document Tag -> Document Tag
addChild _ _ d@(LeafDocument _) = d
addChild key child (BranchDocument d) = BranchDocument d { children = M.insert key child (children d)}

addId :: Mutation -> Key Tag -> Id -> Document Tag -> Document Tag
addId DeleteMutation _ _ d = d
addId _ t i (BranchDocument b) = BranchDocument b
  { presence = M.alter (maybe (Just $ Set.singleton i) (Just . Set.insert i)) (unTag t) $ presence b }
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
          void $ clearElem opDeps key
          modify $ addId opMutation tagged opId
          child <- childGet tagged <$> get
          modify (addChild tagged child)
        assignLeaf other = execState $ do
          let tagged = tagWith RegT (basicKey $ finalKey opCur)
          void $ clear opDeps tagged
          modify $ addId opMutation tagged opId
          child <- childGet tagged <$> get
          case child of
            LeafDocument (RegDocument vals) ->
              modify
              (addChild tagged $
                  LeafDocument $
                  RegDocument (M.insert opId other vals))
            branchChild -> modify (addChild tagged branchChild)

    InsertMutation val -> insert' nextKey
      where
        key = finalKey opCur
        nextKey = next key d
        insertNext =
          let newDoc = applyOp o
                { opCur = setPath mempty . setFinalKey (Key $ I opId) $ opCur
                , opMutation = AssignMutation val
                } d
          in case newDoc of
               BranchDocument b@Branch{branchTag = ListT} -> BranchDocument $ b
                 { keyOrder = M.insert (I opId) (basicKey nextKey) . M.insert (basicKey key) (I opId) $ keyOrder b}
               nd -> nd

        insert' k@(Key (I kid))
          | kid < opId = applyOp (o { opCur = setPath mempty . setFinalKey k $ opCur}) d
        insert' _ = insertNext

    DeleteMutation -> execState (clearElem opDeps (finalKey opCur)) d

  (x :< xs) ->
    let c = childGet x d
        child = applyOp (o {opCur = setPath xs opCur}) c
        newDoc = addId opMutation x opId d
    in addChild x child newDoc
