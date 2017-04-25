{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Crjdt
import Data.Crjdt.Internal

main :: IO ()
main = pure ()

example :: Command Expr
example = do
  x <- bind "x" (key "s" doc)
  pure x
