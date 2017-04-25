{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.FigureSpec where

import Test.Hspec

import Data.Map as Map hiding (insert, keys)
import Data.Set as Set
import Control.Monad.State

import Data.Crjdt as C
import Data.Crjdt.Internal

spec :: Spec
spec = describe "Figures from CRJDT paper" $ do
  let putRemote ops = modify (\ctx -> ctx { received = ops `mappend` (received ctx)})

  it "Figure 1" $ do
    let
      initial = execute (key "key" doc =: "A")
      r1 = initial
      r2 = initial
      r1next = r1 *> execute (key "key" doc =: "C")
      r2next = r2 *> execute (key "key" doc =: "D")

      (fr, firstState) = run 1 r1next
      (sr, secondState) = run 2 r2next

      r1yield = r1next *> putRemote (queue secondState) *> execute yield
      r2yield = r2next *> putRemote (queue firstState) *> execute yield

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
      leaf = RegDocument $ Map.fromList $ [(mkId 2 1, "C"),(mkId 2 2, "D")]

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
          var <- bind "var" (key "colors" doc)
          key "blue" var =: "#0000ff"

        (_, r1result) = run 1 $ execute r1

        r1next = do
          r1
          key "red" "var" =: "#ff0000"

        r2next = putRemote (queue r1result) *> execute yield *> execute (do
            key "colors" doc =: emptyMap
            key "green" (key "colors" doc) =: "#00ff00”"
          )

        (r1r, r1State) = run 1 $ execute (r1next *> keys (key "colors" doc))
        (r2r, r2State) = run 2 (r2next *> execute (keys (key "colors" doc)))

        r1Final = execute r1next *> putRemote (queue r2State) *> execute (yield *> keys (key "colors" doc))
        r2Final = r2next *> putRemote (queue r1State) *> execute (yield *> keys (key "colors" doc))

        (Right keys1, finalResult1) = run 1 r1Final
        (Right keys2, finalResult2) = run 2 r2Final

    keys1 `shouldBe` keys2
    keys1 `shouldBe` Set.fromList ["red", "green"]

    document finalResult1 `shouldBe` document finalResult2
    history finalResult1 `shouldBe` history finalResult2

  it "Figure 3" $ do
    let cmd1 = do
          key "grocery" doc =: emptyList
          C.insert (iter (key "grocery" doc)) "eggs"
          eggs <- bind "eggs" (next (iter (key "grocery" doc)))
          C.insert eggs "ham"

    let cmd2 = do
          key "grocery" doc =: emptyMap
          C.insert (iter (key "grocery" doc)) "milk"
          milk <- bind "milk" (next (iter (key "grocery" doc)))
          C.insert milk "flour"
    let (Right (), r1State) = run 1 $ execute cmd1
        (Right (), r2State) = run 2 $ execute cmd2

    let getValues = do
          eggs <- values "eggs"
          milk <- values (next $ "eggs")
          ham <- values (next $ next $ "eggs")
          flour <- values (next $ next $ next $ "eggs")
          pure (eggs ++ milk ++ ham ++ flour)

    let (Right xs, r1Final) = run 1 (execute cmd1 *> putRemote (queue r2State) *> execute (yield *> getValues))
        (Right (), r2Final) = run 2 (execute cmd2 *> putRemote (queue r1State) *> execute yield)


    xs `shouldBe` ["eggs", "milk", "ham", "flour"]

    document r1Final  `shouldBe` document r2Final
    -- grocery `shouldBe` expectedGrocery

  describe "Empty updates" $ do
    let test what = do
          let cmd = key "g" doc =: what
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
          todo <- "todo" -< iter (key "todo" doc)
          C.insert "todo" emptyMap
          key "title" (next $ "todo") =: "buy milk"
          key "done" (next $ "todo") =: "false"

        (Right (), cmdResult) = run 1 $ execute cmd
        r1next = cmd *> C.delete (next $ "todo")
        r2 = key "done" (next $ iter $ key "todo" doc) =: "true"
        r2next = putRemote (queue cmdResult) *> execute yield *> execute r2
        (Right (), r1St) = run 1 $ execute r1next
        (Right (), r2St) = run 2 $ r2next

        (Right keys1, r1Final) = run 1 (execute r1next *> putRemote (queue r2St) *> execute (yield *> keys (next $ iter $ key "todo" doc)))
        (Right keys2, r2Final) = run 2 (r2next *> putRemote (queue r1St) *> execute (yield *> keys (next $ iter $ key "todo" doc)))

    keys1 `shouldBe` keys2
    keys1 `shouldBe` Set.fromList ["done"]
    document r1Final `shouldBe` document r2Final

  it "Figure 6" $ do
    let cmd = do
          doc =: emptyMap
          list <- bind "list" $ doc .> key "shopping" .> iter
          C.insert list "eggs"
          eggs <- bind "eggs" (next list)
          C.insert eggs "milk"
          C.insert list "cheese"

        (Right xs, _) = run 1 $ execute $ cmd *> do
          eggs <- values "eggs"
          milk <- values ("eggs" .> next)
          cheese <- values ("eggs" .> next .> next)
          pure (eggs ++ milk ++ cheese)

    xs `shouldBe` ["eggs", "milk", "cheese"]
