module DatatypeSpec (spec) where

import Datatype (localise, globalise)
import Example (positiveExamples)

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.List (sort)

import Generate
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "localise/globalise" $ sequence_
    [ it ("are each others' inverses in a sense, on " ++ theName) $ fmap sort (localise (globalise code)) `shouldBe` fmap sort code
    | (theName, code) <- positiveExamples ]
  describe "localise" $ sequence_
    [ it ("is idempotent on " ++ theName) $ localise code `shouldBe` code
    | (theName, code) <- positiveExamples ]
  describe "localise/globalise" $
    prop "are each others' inverses in a sense" $
      forAll randomSD $ \(code,_) -> fmap sort (localise (globalise code)) `shouldBe` fmap sort (localise code)
  describe "globalise/localise" $
    prop "are each others' inverses in a sense" $
      forAll randomSD $ \(code,_) -> fmap sort (globalise (localise code)) `shouldBe` fmap sort (globalise code)
