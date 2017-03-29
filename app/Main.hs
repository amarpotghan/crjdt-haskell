{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Crjdt

main :: IO ()
main = print example

example :: Cmd
example = Let "x" (Keys Doc)
