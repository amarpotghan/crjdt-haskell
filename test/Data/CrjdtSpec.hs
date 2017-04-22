{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.CrjdtSpec where

import Test.Hspec
import Test.Hspec.SmallCheck (property)
import Data.Map as M hiding (empty)
import Data.Either (isRight)
import Test.SmallCheck

import Data.Crjdt
import Data.Crjdt.Internal

eitherToMaybe :: Either x a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe _ = Nothing

sproperty :: Testable IO a => a -> Property IO
sproperty = property . changeDepth (const 3)

spec :: Spec
spec = describe "Crjdt Specs" $ do

  describe "Expr evaluation" $ do
    it "DOC" $
      evalEval 1 Doc `shouldBe` Right (Cursor mempty (Key DocKey))

    it "LET" $ sproperty $ \(expr, name) ->
      let Just cursor = M.lookup name (variables $ execEval 1 (execute (Let (getName name) expr)))
          Right expectedCursor = evalEval 1 expr
      in cursor == expectedCursor

    it "VAR" $ sproperty $ \x expr ->
      let (result, c) = run 1 $ execute (Let (getName x) expr) *> eval (Var x)
          v = M.lookup x (variables c)
      in v == eitherToMaybe result

    it "GET" $ sproperty $ \expr k ->
      let cursor = evalEval 1 (GetKey expr k)
      in k /= (Key Head) && isRight cursor ==> fmap finalKey cursor == Right k