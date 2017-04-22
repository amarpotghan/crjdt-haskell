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
      p = Set.fromList [Id 1 1, Id 1 2, Id 2 1, Id 2 2]
      docPresence = Map.fromList [(Key DocKey, p)]
      keyPresence = Map.fromList [("key", p)]
      leaf = RegDocument $ Map.fromList $ [(Id 2 1, StringLit "C"),(Id 2 2, StringLit "D")]

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
        r2 = r1

        r1Next = r1
          :> Let "var1" (GetKey Doc "colors")
          :> Assign (GetKey (Var "var") "red") (StringLit "#ff0000")

        r2Next = r2
          :> Assign (GetKey Doc "colors") EmptyObject
          :> Let "var2" (GetKey Doc "colors")
          :> Assign (GetKey (Var "var") "green") (StringLit "#00ff00”")

        (r1r, r1State) = run 1 (execute r1Next)
        (r2r, r2State) = run 2 (execute r2Next)

        r1Final = execute r1Next *> putRemote (queue r2State) *> execute Yield
        r2Final = execute r2Next *> putRemote (queue r1State) *> execute Yield

        (_, finalResult1) = run 1 r1Final
        (_, finalResult2) = run 2 r2Final

    r1r `shouldBe` Right ()
    r2r `shouldBe` Right ()

    document finalResult1 `shouldBe` document finalResult2
    history finalResult1 `shouldBe` history finalResult2
