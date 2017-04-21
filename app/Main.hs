{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Crjdt
import Data.Crjdt.Internal

main :: IO ()
main = print example

example :: Cmd
example = Let "x" (Var "y")
