{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.FigureSpec where

import Test.Hspec

import Data.Map as Map hiding (insert)
import Data.Set as Set
import Control.Monad.State

import Data.Crjdt as C
import Data.Crjdt.Internal

spec :: Spec
spec = describe "Figures from CRJDT paper" $ do
  let putRemote ops = modify (\ctx -> ctx { received = ops `mappend` (received ctx)})

  it "Figure 1" $ do
    let
      initial = execute (keyOf doc "key" =: string "A")
      r1 = initial
      r2 = initial
      r1nextOf = r1 *> execute (keyOf doc "key" =: string "C")
      r2nextOf = r2 *> execute (keyOf doc "key" =: string "D")

      (fr, firstState) = run 1 r1nextOf
      (sr, secondState) = run 2 r2nextOf

      r1yield = r1nextOf *> putRemote (queue secondState) *> execute yield
      r2yield = r2nextOf *> putRemote (queue firstState) *> execute yield

      (rr, r1Result) = run 1 r1yield
      (r2r, r2Result) = run 2 r2yield

    _ <- traverse (\x -> x `shouldBe` Right ()) [fr, sr, rr, r2r]

    document firstState `shouldSatisfy` (/= (document secondState))
    document r1Result `shouldBe` document r2Result
    history r1Result `shouldBe` history r2Result

    let
      p = Set.fromList [mkId 1 1, mkId 1 2, mkId 2 1, mkId 2 2]
      docPresence = Map.fromList [(Key DocKey, p)]
      keyPresence = Map.fromList [("key", p)]
      leaf = RegDocument $ Map.fromList $ [(mkId 2 1, string "C"),(mkId 2 2, string "D")]

      innerMap = Branch
        { children = Map.fromList [(tagWith RegT (Str "key"), LeafDocument leaf)]
        , presence = keyPresence
        , keyOrder = mempty
        , branchTag = MapT
        }

      parent = Branch
        { children = Map.fromList [(tagWith MapT DocKey, BranchDocument innerMap)]
        , presence = docPresence
        , keyOrder = mempty
        , branchTag = MapT
        }

      d = BranchDocument parent

    document r1Result `shouldBe` d

  it "Figure 2" $ do
    let r1 = do
          var <- bind "var" (keyOf doc "colors")
          keyOf var "blue" =: string "#0000ff"

        (_, r1result) = run 1 $ execute r1

        r1nextOf = do
          r1
          keyOf (var "var") "red" =: string "#ff0000"

        r2nextOf = putRemote (queue r1result) *> execute yield *> execute (do
            keyOf doc "colors" =: emptyMap
            keyOf (keyOf doc "colors") "green" =: string "#00ff00”"
          )

        (r1r, r1State) = run 1 (execute r1nextOf *> keysOf (keyOf doc "colors"))
        (r2r, r2State) = run 2 (r2nextOf *> keysOf (keyOf doc "colors"))

        r1Final = execute r1nextOf *> putRemote (queue r2State) *> execute yield *> keysOf (keyOf doc "colors")
        r2Final = r2nextOf *> putRemote (queue r1State) *> execute yield *> keysOf (keyOf doc "colors")

        (Right keys1, finalResult1) = run 1 r1Final
        (Right keys2, finalResult2) = run 2 r2Final

    keys1 `shouldBe` keys2
    keys1 `shouldBe` Set.fromList ["red", "green"]

    document finalResult1 `shouldBe` document finalResult2
    history finalResult1 `shouldBe` history finalResult2

  it "Figure 3" $ do
    let cmd1 = do
          keyOf doc "grocery" =: emptyList
          C.insert (iter (keyOf doc "grocery")) (string "eggs")
          eggs <- bind "eggs" (nextOf (iter (keyOf doc "grocery")))
          C.insert eggs (string "ham")

    let cmd2 = do
          keyOf doc "grocery" =: emptyMap
          C.insert (iter (keyOf doc "grocery")) (string "milk")
          milk <- bind "milk" (nextOf (iter (keyOf doc "grocery")))
          C.insert milk (string "flour")
    let (Right (), r1State) = run 1 $ execute cmd1
        (Right (), r2State) = run 2 $ execute cmd2

    let getValues = do
          eggs <- valuesOf (var "eggs")
          milk <- valuesOf (nextOf $ var "eggs")
          ham <- valuesOf (nextOf $ nextOf $ var "eggs")
          flour <- valuesOf (nextOf $ nextOf $ nextOf $ var "eggs")
          pure (eggs ++ milk ++ ham ++ flour)

    let (Right xs, r1Final) = run 1 (execute cmd1 *> putRemote (queue r2State) *> execute yield *> getValues)
        (Right (), r2Final) = run 2 (execute cmd2 *> putRemote (queue r1State) *> execute yield)


    xs `shouldBe` [string "eggs", string "milk", string "ham", string "flour"]

    document r1Final  `shouldBe` document r2Final
    -- grocery `shouldBe` expectedGrocery

  describe "Empty updates" $ do
    let test what = do
          let cmd = keyOf doc "g" =: what
              cmd1 = cmd
              (Right (), r) = run 1 $ execute cmd
              (Right (), r1) = run 2 $ execute cmd1

          let (Right (), x1) = run 1 (execute cmd *> putRemote (queue r1) *> execute yield)
              (Right (), x2) = run 2 (execute cmd1 *> putRemote (queue r) *> execute yield)

          document x1 `shouldBe` document x2

    it "Empty object update" $ test emptyMap
    it "Empty list update" $ test emptyList

  it "Figure 4" $ do
    let cmd = do
          todo <- "todo" -< iter (keyOf doc "todo")
          C.insert (var "todo") emptyMap
          keyOf (nextOf $ var "todo") "title" =: string "buy milk"
          keyOf (nextOf $ var "todo") "done" =: string "false"

        (Right (), cmdResult) = run 1 $ execute cmd
        r1nextOf = cmd *> C.delete (nextOf $ var "todo")
        r2 = keyOf (nextOf $ iter $ keyOf doc "todo") "done" =: string "true"
        r2nextOf = putRemote (queue cmdResult) *> execute yield *> execute r2
        (Right (), r1St) = run 1 $ execute r1nextOf
        (Right (), r2St) = run 2 $ r2nextOf

        (Right keys1, r1Final) = run 1 (execute r1nextOf *> putRemote (queue r2St) *> execute yield *> keysOf (nextOf $ iter $ keyOf doc "todo"))
        (Right keys2, r2Final) = run 2 (r2nextOf *> putRemote (queue r1St) *> execute yield *> keysOf (nextOf $ iter $ keyOf doc "todo"))

    keys1 `shouldBe` keys2
    keys1 `shouldBe` Set.fromList ["done"]
    document r1Final `shouldBe` document r2Final

  it "Figure 6" $ do
    let cmd = do
          doc =: emptyMap
          list <- bind "list" (iter (keyOf doc "shopping"))
          C.insert list (string "eggs")
          eggs <- bind "eggs" (nextOf list)
          C.insert eggs (string "milk")
          C.insert list (string "cheese")

        (Right xs, _) = run 1 $ (execute cmd) *> do
          eggs <- valuesOf (var "eggs")
          milk <- valuesOf (nextOf (var "eggs"))
          cheese <- valuesOf (nextOf (nextOf (var "eggs")))
          pure (eggs ++ milk ++ cheese)

    xs `shouldBe` [string "eggs", string "milk", string "cheese"]
