{-# LANGUAGE NamedFieldPuns #-}
module DatatypeSpec (spec, connectionsEmpty) where

import Datatype (StateDiagram(..), UMLStateDiagram, localise, globalise)
import ExampleSpec (positiveExamples)

import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad (void)

import Generate
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "localise/globalise" $ void $ sequence
    [ it ("are each others' inverses in a sense, on " ++ name) $ localise (globalise code) `shouldBe` localise code
    | (name, code) <- positiveExamples ]
  describe "globalise/localise" $ void $ sequence
    [ it ("are each others' inverses in a sense, on " ++ name) $ globalise (localise code) `shouldBe` globalise code
    | (name, code) <- positiveExamples ]
  describe "localise/globalise" $
    prop ("are each others' inverses in a sense") $
      forAll randomSD $ \(code,_) -> localise (globalise code) `shouldBe` localise code
  describe "globalise/localise" $
    prop ("are each others' inverses in a sense") $
      forAll randomSD $ \(code,_) -> globalise (localise code) `shouldBe` globalise code
  describe "globalise" $ void $ sequence
    [ it ("doesn't leave any connections inside, on " ++ name) $ all connectionsEmpty (substate (globalise code))
    | (name, code) <- positiveExamples ]
  describe "globalise" $
    prop ("doesn't leave any connections inside") $
      forAll randomSD $ \(code,_) -> all connectionsEmpty (substate (globalise code))

connectionsEmpty :: UMLStateDiagram -> Bool
connectionsEmpty = all null
