{-# OPTIONS_GHC -Wno-error=deprecations #-}

module DatatypeSpec (spec) where

import Datatype (localise, globalise, UMLStateDiagram(unUML'), umlStateDiagram)
import Datatype.ClassInstances ()
import Example (positiveExamples)

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.List (sort)

import Generate
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "localise/globalise" $ sequence_
    [ it ("are each others' inverses in a sense, on " ++ theName) $ fmap sort (localise (unUML' (globalise code))) `shouldBe` (fmap sort . unUML') code
    | (theName, code) <- positiveExamples ]
  describe "localise" $ sequence_
    [ it ("is idempotent on " ++ theName) $ localise (unUML' code) `shouldBe` unUML' code
    | (theName, code) <- positiveExamples ]
  describe "localise/globalise" $
    prop "are each others' inverses in a sense" $
      forAll randomSD $ \(code,_) -> fmap sort (localise (unUML' (globalise code))) `shouldBe` fmap sort (localise (unUML' code))
  describe "globalise/localise" $
    prop "are each others' inverses in a sense" $
      forAll randomSD $ \(code,_) -> (fmap sort . unUML') (globalise (umlStateDiagram (localise (unUML' code)))) `shouldBe` (fmap sort . unUML') (globalise code)
