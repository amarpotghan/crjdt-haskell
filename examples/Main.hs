{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Crjdt as C

main :: IO ()
main = do
  (r1, r2) <- sync replica1 replica2
  let replica1' = execEval 1 r1
      replica2' = execEval 2 r2
  -- Both replicas converge to: {"key": {"B", "C"}}
  print (document replica1' == document replica2') -- True

original :: Command ()
original = (doc .> key "key") =: "A"

replica1 :: Command ()
replica1 = do
  original
  (doc .> key "key") =: "B"

replica2 :: Command ()
replica2 = do
  original
  (doc .> key "key") =: "C"
