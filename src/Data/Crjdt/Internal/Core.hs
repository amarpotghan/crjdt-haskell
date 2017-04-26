{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Crjdt.Internal.Core where

import Test.SmallCheck.Series
import Data.Text
import Data.Void
import Data.Set (Set)
import Control.Monad.Free
import Data.String

import Data.Crjdt.Types

data Val
  = Number Int
  | StringLit Text
  | BoolLit Bool
  | Null
  | EmptyObject
  | EmptyArray
  deriving (Show, Eq)

instance IsString Val where
  fromString = StringLit . pack

prettyVal :: Val -> String
prettyVal (Number i) = show i
prettyVal (BoolLit i) = show i
prettyVal (StringLit i) = show i
prettyVal x = show x

instance Monad m => Serial m Val where
  series =
    cons1 Number \/
    (StringLit . pack <$> series) \/
    cons1 BoolLit \/
    cons0 Null \/
    cons0 EmptyObject \/
    cons0 EmptyArray

data Cmd a
  = Let !Text !Expr (Expr -> a)
  | Assign !Expr !Val a
  | InsertAfter !Expr !Val a
  | Delete !Expr a
  | Values !Expr ([Val] -> a)
  | Keys !Expr (Set (Key Void) -> a)
  | Yield a
  deriving Functor

type Command a = Free Cmd a

data Expr
  = Doc
  | Var !Var
  | Iter !Expr
  | Next !Expr
  | GetKey !Expr !(Key Void)
  deriving (Show, Eq)

instance IsString Expr where
  fromString = Var . fromString

instance Monad m => Serial m Expr where
  series =
    -- cons1 Var \/
    cons0 Doc \/
    cons1 Iter \/
    cons1 Next \/
    cons2 GetKey
