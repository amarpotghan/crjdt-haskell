{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Crjdt.Internal.Core where

import Test.SmallCheck.Series
import Data.Text
import Data.Void
import Data.Crjdt.Types

data Val
  = Number Int
  | StringLit Text
  | BoolLit Bool
  | Null
  | EmptyObject
  | EmptyArray
  deriving (Show, Eq)

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

data Cmd
  = Let !Text !Expr
  | Assign !Expr !Val
  | InsertAfter !Expr !Val
  | Delete !Expr
  | Yield
  | !Cmd :> !Cmd
  deriving (Show, Eq)

infixr 6 :>

data Expr
  = Doc
  | Var !Var
  -- | Keys Expr
  -- | Values Expr
  | Iter !Expr
  | Next !Expr
  | GetKey !Expr !(Key Void)
  deriving (Show, Eq)

instance Monad m => Serial m Expr where
  series =
    -- cons1 Var \/
    cons0 Doc \/
    cons1 Iter \/
    cons1 Next \/
    cons2 GetKey
