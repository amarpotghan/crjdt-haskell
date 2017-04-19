module Data.CrjdtSpec where

import Test.Hspec

import Data.Crjdt

spec :: Spec
spec = describe "Crjdt Specs" $ do
  describe "Expr evaluation" $ do
    it "doc" $ evalEval 1 Doc `shouldBe` Right (Cursor mempty (Key DocKey))
