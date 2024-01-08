{-# OPTIONS_GHC -Wno-error=deprecations #-}

module Modelling.StateDiagram.GenerateSpec (spec) where

import Modelling.StateDiagram.Generate
import Modelling.StateDiagram.Datatype (localise, globalise, UMLStateDiagram(unUML'), umlStateDiagram)
import Modelling.StateDiagram.Datatype.ClassInstances ()
import Modelling.StateDiagram.Checkers (allTheCheckers)

import Test.QuickCheck
import Data.List.Extra

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "randomSD" $
    prop "generates valid diagram expressions" $
      forAll randomSD $ \(code,_) -> firstJust id (map (($ code) . snd) $ filter ((`notElem` ["checkSemantics", "checkDrawability"]) . fst) allTheCheckers) `shouldBe` Nothing
  describe "randomSD" $
    prop "generates valid diagram expressions after 'localise'" $
      forAll randomSD $ \(code,_) -> firstJust id (map (($ umlStateDiagram (localise (unUML' code))) . snd) allTheCheckers) `shouldBe` Nothing
  describe "randomSD" $
    prop "generates valid diagram expressions after 'globalise'" $
      forAll randomSD $ \(code,_) -> firstJust id (map (($ globalise code) . snd) allTheCheckers) `shouldBe` Nothing
  describe "randomSD" $
    prop "does not retry too often" $
      forAll randomSD $ \(_,attempts) -> attempts `shouldSatisfy` (< 100)
