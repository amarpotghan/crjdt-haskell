module Data.Crjdt
  ( module Core
  , bind
  , (-<)
  , iter
  , next
  , key
  , doc
  , var
  , insert
  , delete
  , emptyMap
  , emptyList
  , (=:)
  , string
  , yield
  , keys
  , values
  , Void
  , (.>)
  ) where

import Data.Text as T
import Data.Set (Set)
import Data.Void
import Control.Monad.Free (liftF)

import Data.Crjdt.Context as Core
import Data.Crjdt.Types as Core
import Data.Crjdt.Eval as Core
import Data.Crjdt.Internal

(.>) :: b -> (b -> a) -> a
(.>) b f = f b

emptyMap, emptyList :: Val
emptyMap = EmptyObject
emptyList = EmptyArray

yield :: Command ()
yield = liftF (Yield ())

assign, (=:) :: Expr -> Val -> Command ()
assign e v = liftF (Assign e v ())
(=:) = assign

bind, (-<) :: Text -> Expr -> Command Expr
bind t e = liftF (Let t e id)
(-<) = bind

string :: Text -> Val
string = StringLit

iter :: Expr -> Expr
iter = Iter

next :: Expr -> Expr
next = Next

key :: Key Void -> Expr -> Expr
key = flip GetKey

doc :: Expr
doc = Doc

var :: Text -> Expr
var = Var . Variable

insert :: Expr -> Val -> Command ()
insert e v = liftF (InsertAfter e v ())

delete :: Expr -> Command ()
delete e = liftF (Delete e ())

keys :: Expr -> Command (Set (Key Void))
keys e = liftF (Keys e id)

values :: Expr -> Command [Val]
values e = liftF (Values e id)
