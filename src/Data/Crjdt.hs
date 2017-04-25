module Data.Crjdt
  ( module Core
  , bind
  , (-<)
  , iter
  , nextOf
  , keyOf
  , doc
  , var
  , insert
  , delete
  , emptyMap
  , emptyList
  , (=:)
  , string
  , yield
  ) where

import Data.Crjdt.Context as Core
import Data.Crjdt.Types as Core
import Data.Crjdt.Eval as Core

import Data.Crjdt.Internal
import Data.Text as T
import Data.Void

emptyMap, emptyList :: Val
emptyMap = EmptyObject
emptyList = EmptyArray

yield :: Cmd
yield = Yield

assign, (=:) :: Expr -> Val -> Cmd
assign = Assign
(=:) = assign

bind, (-<) :: Text -> Expr -> Cmd
bind = Let
(-<) = bind

string :: Text -> Val
string = StringLit

iter :: Expr -> Expr
iter = Iter

nextOf :: Expr -> Expr
nextOf = Next

keyOf :: Expr -> Key Void -> Expr
keyOf = GetKey

doc :: Expr
doc = Doc

var :: Text -> Expr
var = Var . Variable

insert :: Expr -> Val -> Cmd
insert = InsertAfter

delete :: Expr -> Cmd
delete = Delete
