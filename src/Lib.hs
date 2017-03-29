{-# LANGUAGE DeriveFunctor #-}
module Lib
  ( someFunc
  ) where

import Data.Set as Set
import Data.Text
import Control.Monad.Free

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Val
  = Number Integer
  | Str Text
  | Boolean Bool
  | LiteralObject
  | LiteralList

data Var = Var Text
data Key = Key Text

data Node

data ExprF a
  = Get Key a (Node -> a)
  | Iterate a (ListNode -> a)
  | Next a (Node -> a)
  | Keys a (Set.Set Key -> a)
  | Values a ([Val] -> a)
  | Let Var a (Var -> a)
  deriving (Functor)

type Expr a = Free (ExprF a)
