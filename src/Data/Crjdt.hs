{-# LANGUAGE FlexibleContexts #-}
-- |This module exports all necessarry data types and functions for
-- expressing and executing commands which allow modifying and
-- querying the state of JSON in local replica

module Data.Crjdt
  (
  -- * Expressions
    iter
  , next
  , key
  , doc
  , var

  -- * Commands
  , Command(..)
  , yield
  , keys
  , values
  , insert
  , delete
  , bind
  , (-<)
  , assign
  , (=:)

  -- * Values
  , string
  , emptyMap
  , emptyList

  -- * Evaluation and Execution
  , Eval.eval
  , Eval.execute

  -- * Re-exports
  , Void
  , (.>)
  , (&)

  -- * Others
  , sync
  , module Core
  ) where

import Data.Text as T
import Data.Set (Set)
import Data.Void
import Data.Function
import Control.Monad.Free (liftF)

import Data.Crjdt.Context as Core
import Data.Crjdt.Types as Core

import qualified Data.Crjdt.Eval as Eval (execute, eval)
import Data.Crjdt.Eval as Core hiding (execute, eval)

import Data.Crjdt.Internal

(.>) :: b -> (b -> a) -> a
(.>) = (&)

infixl 4 .>

-- |'emptyMap' corresponds to {}.
emptyMap :: Val
emptyMap = EmptyObject

-- |'emptyList' corresponds to [].
emptyList :: Val
emptyList = EmptyArray

-- |Apply commands received from other replicas to current replica.
yield :: Command ()
yield = liftF (Yield ())

{-| String literal.

  @
    'doc' '.>' "planet" '.=' 'string' \"Earth\"
  @

-}
string :: Text -> Val
string = StringLit

{-| Starts iterating over the list.

  @
    do
     let planetList = 'doc' '.>' "planet" .> 'iter'
     insert \"Earth\" planetList
  @

-}
iter :: Expr -> Expr
iter = Iter

{-| Moves to the next element in the list.
-}
next :: Expr -> Expr
next = Next

{-| Get value of given key in given expression.
-}
key :: Key Void -> Expr -> Expr
key = flip GetKey

{-| The root of the JSON object.
-}
doc :: Expr
doc = Doc

-- |Variable.
var :: Text -> Expr
var = Var . Variable

-- |Insert a @Val@ in the list.
insert :: Val -> Expr -> Command ()
insert v e = liftF (InsertAfter e v ())

-- |Delete the list element.
delete :: Expr -> Command ()
delete e = liftF (Delete e ())

-- |Get all keys of object pointed by given 'Expr'.
keys :: Expr -> Command (Set (Key Void))
keys e = liftF (Keys e id)

-- |Get all values of object pointed by given 'Expr'.
values :: Expr -> Command [Val]
values e = liftF (Values e id)

-- |Assign value to the key pointed by given 'Expr'.
assign, (=:) :: Expr -> Val -> Command ()
assign e v = liftF (Assign e v ())
(=:) = assign

infixr 5 =:

-- |Let binding.
bind, (-<) :: Text -> Expr -> Command Expr
bind t e = liftF (Let t e id)
(-<) = bind

-----------------------------------------------------------------------------------------------
-- Utility functions

sync :: (ReplicaId, Command ()) -> (ReplicaId, Command ()) -> Either EvalError (Eval (), Eval ())
sync (rid1, first) (rid2, second) =
  let (rFirst, sFirst) = run rid1 (Eval.execute first)
      (rSecond, sSecond) = run rid2 (Eval.execute second)
      synced which replica = do
        Eval.execute which
        addReceivedOps (queue replica)
        Eval.execute yield
  in case (rFirst *> rSecond) of
    Right () -> pure $ (synced first sSecond, synced second sFirst)
    Left ex -> Left ex
