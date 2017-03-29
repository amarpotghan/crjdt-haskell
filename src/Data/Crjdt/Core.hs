module Data.Crjdt.Core where

import Data.Text
import Data.String

data Var = Variable Text deriving (Show, Eq)

instance IsString Var where
  fromString = Variable . fromString

type Key = Text

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
  | GetKey Expr Key
  deriving (Show, Eq)
