{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
  | At Expr Int
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

untaggedKey :: Text -> Key Void
untaggedKey = Key

tagWith :: tag -> Text -> Key tag
tagWith t = TaggedKey . TK t

data Cursor = Cursor
  { path :: Seq.Seq (Key Tag)
  , finalKey :: Key Void
  } deriving (Show, Eq)

data Context = Context

data EvalError = GetOnHead

newtype Eval a
  = Eval { runEval :: ExceptT EvalError (State Context) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadFix
           , MonadError EvalError
           )

doTag :: a -> Key Void -> Key a
doTag given (Key key) = Key given key

appendWith :: Tag -> Key Void -> Cursor -> Cursor
appendWith tag key (Cursor path final) = Seq (path `mappend` Seq.singleton (doTag tag final)) key

eval :: (MonadError EvalError m, MonadState Context m) => Expr -> m Result
eval Doc = pure $ Cursor Seq.empty doc
eval (GetKey expr key) = do
  cursor <- eval expr
  case finalKey cursor of
    (Key "head") -> throwError GetOnHead
    _ -> pure (appendWith key cursor)
