{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Crjdt
import Data.Crjdt.Internal

main :: IO ()
main = pure ()

example :: Command Expr
example = do
  x <- bind "x" (keyOf doc "s")
  pure x
