[![Build Status](https://travis-ci.org/amarpotghan/crjdt-haskell.svg?branch=master)](https://travis-ci.org/amarpotghan/haskell-crjdt)

# A Conflict-Free Replicated JSON Datatype for Haskell

*crjdt-haskell* provides high level interface to CRDT which formalised in the [paper](https://arxiv.org/pdf/1608.03960v1.pdf) by Martin Kleppmann and Alastair R. Beresford.

## Future work

* Aeson support
* Simplify API as described in [second version](https://arxiv.org/abs/1608.03960) of the paper

## Example

```haskell

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

```

## LICENSE

Copyright © 2017 Amar Potghan

Distributed under BSD License.
