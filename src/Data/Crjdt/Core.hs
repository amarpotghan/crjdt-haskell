{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Crjdt.Core where

import Data.Text
import Data.String
import Data.Void
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Fail
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import Data.Sequence (ViewL(..), viewl)
import qualified Data.Sequence as Seq

data Var = Variable Text deriving (Show, Eq)

instance IsString Var where
  fromString = Variable . fromString

data TaggedKey tag = TK
  { tag :: tag
  , key :: Text
  } deriving (Show, Eq)

data Key tag where
  Key :: Text -> Key Void
  TaggedKey :: TaggedKey tag -> Key tag

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
  deriving (Show, Eq)

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
doc :: Key Void
doc = Key "doc"

tagWith :: tag -> Text -> Key tag
tagWith t = TaggedKey . TK t

data Cursor = Cursor
  { path :: Seq.Seq (Key Tag)
  , finalKey :: Key Void
  } deriving (Show, Eq)

data Context = Context

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

evalEval :: Expr -> Either EvalError Cursor
evalEval = (`evalState` Context) . runExceptT . runEval . eval

doTag :: a -> Key Void -> Key a
doTag given (Key k) = tagWith given k
doTag given (TaggedKey (TK _ k)) = tagWith given k

appendWith :: Tag -> Key Void -> Cursor -> Cursor
appendWith t k (Cursor p final) = Cursor (p `mappend` Seq.singleton (doTag t final)) k

lookupCtx :: Var -> Context -> Maybe Cursor
lookupCtx _ _ = Nothing

alreadyPresent :: Key Void -> Context -> Bool
alreadyPresent _ _ = False

next :: Key Void -> Context -> Key Void
next _ _ = "tail"

-- Avoiding Lens dependency for now
setFinalKey :: Cursor -> Key Void -> Cursor
setFinalKey c fk = c { finalKey = fk }

setPath :: Cursor -> Seq.Seq (Key Tag) -> Cursor
setPath c newpath = c { path = newpath }

type Ctx m = (MonadError EvalError m, MonadState Context m)

findChild :: Key Tag -> Context -> Maybe Document
findChild _ _ = Nothing

data Document

stepNext :: Ctx m => Document -> Cursor -> m Cursor
stepNext d c@(Cursor (viewl -> Seq.EmptyL) (next -> getNextKey)) = get >>= \ctx -> do
  let nextKey = getNextKey ctx
  case (nextKey /= tailKey, alreadyPresent nextKey ctx) of
    (True, True) -> pure (Cursor Seq.empty nextKey)
    (True, False) -> stepNext d (Cursor Seq.empty nextKey)
    (False, _) -> pure c
stepNext d c@(Cursor (viewl -> (x :< xs)) _) = get >>= f
  where f = maybe (pure c) (fmap (setFinalKey c . finalKey) . (`stepNext` (setPath c xs))) . findChild x
stepNext _ c = pure c

topDoc :: Document
topDoc = undefined

eval :: Ctx m => Expr -> m Result
eval Doc = pure $ Cursor Seq.empty doc
eval (GetKey expr k) = do
  cursor <- eval expr
  case finalKey cursor of
    (Key "head") -> throwError GetOnHead
    _ -> pure (appendWith MapT k cursor)
eval (Var var) = get >>= maybe (throwError (UndefinedVariable var)) pure . lookupCtx var
eval (Iter expr) = appendWith ListT headKey <$> eval expr
eval (Next expr) = eval expr >>= stepNext topDoc
-- TODO: query part. Not yet implemented
eval values = eval values
