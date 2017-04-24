{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.FigureSpec where

import Test.Hspec

import Data.Map as Map
import Data.Set as Set
import Control.Monad.State

import Data.Crjdt
import Data.Crjdt.Internal

spec :: Spec
spec = describe "Figures from CRJDT paper" $ do
  let putRemote ops = modify (\ctx -> ctx { received = ops `mappend` (received ctx)})

  it "Figure 1" $ do
    let
      initial = execute (Assign (GetKey Doc "key") (StringLit "A"))
      r1 = initial
      r2 = initial
      r1Next = r1 *> execute (Assign (GetKey Doc "key") (StringLit "C"))
      r2Next = r2 *> execute (Assign (GetKey Doc "key") (StringLit "D"))

      (fr, firstState) = run 1 r1Next
      (sr, secondState) = run 2 r2Next

      r1Yield = r1Next *> putRemote (queue secondState) *> execute Yield
      r2Yield = r2Next *> putRemote (queue firstState) *> execute Yield

      (rr, r1Result) = run 1 r1Yield
      (r2r, r2Result) = run 2 r2Yield

    _ <- traverse (\x -> x `shouldBe` Right ()) [fr, sr, rr, r2r]

    document firstState `shouldSatisfy` (/= (document secondState))
    document r1Result `shouldBe` document r2Result
    history r1Result `shouldBe` history r2Result

    let
      p = Set.fromList [mkId 1 1, mkId 1 2, mkId 2 1, mkId 2 2]
      docPresence = Map.fromList [(Key DocKey, p)]
      keyPresence = Map.fromList [("key", p)]
      leaf = RegDocument $ Map.fromList $ [(mkId 2 1, StringLit "C"),(mkId 2 2, StringLit "D")]

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
    let r1 = Let "var" (GetKey Doc "colors")
          :> Assign (GetKey (Var "var") "blue") (StringLit "#0000ff")
        (_, r1result) = run 1 $ execute r1

        r1Next = r1
          :> Assign (GetKey (Var "var") "red") (StringLit "#ff0000")

        r2Next = putRemote (queue r1result) *> execute Yield *> execute (
          Assign (GetKey Doc "colors") EmptyObject
          :> Assign (GetKey (GetKey Doc "colors") "green") (StringLit "#00ff00”"))

        (r1r, r1State) = run 1 (execute r1Next *> keysOf (GetKey Doc "colors"))
        (r2r, r2State) = run 2 (r2Next *> keysOf (GetKey Doc "colors"))

        r1Final = execute r1Next *> putRemote (queue r2State) *> execute Yield *> keysOf (GetKey Doc "colors")
        r2Final = r2Next *> putRemote (queue r1State) *> execute Yield *> keysOf (GetKey Doc "colors")

        (Right keys1, finalResult1) = run 1 r1Final
        (Right keys2, finalResult2) = run 2 r2Final

    keys1 `shouldBe` keys2
    keys1 `shouldBe` Set.fromList ["red", "green"]

    document finalResult1 `shouldBe` document finalResult2
    history finalResult1 `shouldBe` history finalResult2

  it "Figure 3" $ do
    let cmd1 = Assign (GetKey Doc "grocery") EmptyArray
          :> InsertAfter (Iter (GetKey Doc "grocery")) (StringLit "eggs")
          :> Let "eggs" (Next (Iter (GetKey Doc "grocery")))
          :> InsertAfter (Var "eggs") (StringLit "ham")

    let cmd2 = Assign (GetKey Doc "grocery") EmptyArray
          :> InsertAfter (Iter (GetKey Doc "grocery")) (StringLit "milk")
          :> Let "milk" (Next (Iter (GetKey Doc "grocery")))
          :> InsertAfter (Var "milk") (StringLit "flour")
    let (Right (), r1State) = run 1 $ execute cmd1
        (Right (), r2State) = run 2 $ execute cmd2

    let getValues = do
          eggs <- valuesOf (Var "eggs")
          milk <- valuesOf (Next $ Var "eggs")
          ham <- valuesOf (Next $ Next $ Var "eggs")
          flour <- valuesOf (Next $ Next $ Next $ Var "eggs")
          pure (eggs ++ milk ++ ham ++ flour)

    let (Right xs, r1Final) = run 1 (execute cmd1 *> putRemote (queue r2State) *> execute Yield *> getValues)
        (Right (), r2Final) = run 2 (execute cmd2 *> putRemote (queue r1State) *> execute Yield)


    xs `shouldBe` [StringLit "eggs", StringLit "milk", StringLit "ham", StringLit "flour"]

    document r1Final  `shouldBe` document r2Final
    -- grocery `shouldBe` expectedGrocery

  describe "Empty updates" $ do
    let test what = do
          let cmd = Assign (GetKey Doc "g") what
              cmd1 = cmd
              (Right (), r) = run 1 $ execute cmd
              (Right (), r1) = run 2 $ execute cmd1

          let (Right (), x1) = run 1 (execute cmd *> putRemote (queue r1) *> execute Yield)
              (Right (), x2) = run 2 (execute cmd1 *> putRemote (queue r) *> execute Yield)

          document x1 `shouldBe` document x2

    it "Empty object update" $ test EmptyObject
    it "Empty list update" $ test EmptyArray

  it "Figure 6" $ do
    let cmd = Assign Doc EmptyObject
          :> Let "list" (Iter (GetKey Doc "shopping"))
          :> InsertAfter (Var "list") (StringLit "eggs")
          :> Let "eggs" (Next (Var "list"))
          :> InsertAfter (Var "eggs") (StringLit "milk")
          :> InsertAfter (Var "list") (StringLit "cheese")
        (Right xs, _) = run 1 $ (execute cmd) *> do
          eggs <- valuesOf (Var "eggs")
          milk <- valuesOf (Next (Var "eggs"))
          cheese <- valuesOf (Next (Next (Var "eggs")))
          pure (eggs ++ milk ++ cheese)

    xs `shouldBe` [StringLit "eggs", StringLit "milk", StringLit "cheese"]
