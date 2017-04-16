{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Crjdt.Core where

import Data.Text
import Data.String
import Data.Void
import Data.Maybe
import Data.Monoid
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Except
import Data.Sequence (ViewL(..), viewl)
import Data.Map as M
import Data.Set as Set
import qualified Data.Sequence as Seq

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
  | Yeild
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
doc :: Key Tag
doc = tagWith MapT "doc"

tagWith :: tag -> Text -> Key tag
tagWith t = TaggedKey . TK t

data Cursor = Cursor
  { path :: Seq.Seq (Key Tag)
  , finalKey :: Key Void
  } deriving (Show, Eq)

data Context = Context
  { document :: Document Tag
  , variables :: M.Map Var Cursor
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

initial :: Context
initial = Context
  { document = BranchDocument (Branch mempty mempty MapT)
  , variables = mempty
  }

evalEval :: Expr -> Either EvalError Cursor
evalEval = (`evalState` initial) . runExceptT . runEval . eval

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

data Document tag
  = BranchDocument (Branch tag)
  | LeafDocument RegDocument
  deriving (Functor, Foldable, Traversable)

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
eval Doc = pure $ Cursor Seq.empty $ unTag doc
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
