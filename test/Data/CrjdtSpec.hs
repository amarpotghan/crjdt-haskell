{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.CrjdtSpec where

import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Map as M hiding (empty)
import Data.Either (isRight)
import Data.Maybe
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import Control.Applicative (liftA2)
import Control.Monad

import Data.Crjdt
import Data.Crjdt.Internal

eitherToMaybe :: Either x a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe _ = Nothing

-- allProps :: IO ()
-- allProps = traverse_ check [property_let, property_var]

twenty = Range.linear 0 20

keyGen :: Monad m => Gen m BasicKey
keyGen = Gen.choice $ (pure <$> [Head, Tail, DocKey]) ++
  [ I . Id <$> (liftA2 (fmap (bimap toInteger toInteger) . (,)) positiveInts positiveInts)
  , Str <$> Gen.text twenty Gen.hexit
  ]

positiveInts :: Monad m => Gen m Int
positiveInts = Gen.int (Range.linear 0 1000)

exprGen :: Gen IO Expr
exprGen = Gen.recursive Gen.choice terminal nonterminal
  where
    terminal = [pure doc]
    letVar = Gen.text twenty Gen.hexit >>= \t -> Let t <$> exprGen
    nonterminal =
      [ iter <$> exprGen
      , next <$> exprGen
      , key <$> (Key <$> keyGen) <*> exprGen
      ]

property_let :: Property
property_let = property $ do
  expr <- forAll exprGen
  name <- Variable <$> (forAll $ Gen.text twenty Gen.hexit)
  let (result, state) = run 1 $ execute $ bind (getName name) expr
  when (isRight result) $ do
    let Just cursor = M.lookup name $ variables $ state
        Right expectedCursor = evalEval 1 expr
    cursor === expectedCursor

property_var :: Property
property_var = property $ do
  expr <- forAll exprGen
  x <- Variable <$> (forAll $ Gen.text twenty Gen.hexit)
  let (result, c) = run 1 $ execute (bind (getName x) expr) *> eval (Var x)
      v = M.lookup x (variables c)
  v === eitherToMaybe result

property_get :: Property
property_get = property $ do
  expr <- forAll exprGen
  k <- forAll keyGen
  let cursor = evalEval 1 (GetKey expr $ Key k)
  when (k /= Head && isRight cursor) $ fmap finalKey cursor === Right (Key k)

checkH :: Property -> IO ()
checkH = void . check

spec :: Spec
spec = describe "Crjdt Specs" $
  describe "Expr evaluation" $ do
    it "DOC" $ evalEval 1 Doc `shouldBe` Right (Cursor mempty (Key DocKey))
    it "LET" $ checkH property_let
    it "VAR" $ checkH property_var
    it "GET" $ checkH property_get
