module PlantUMLDiagramsSpec (spec) where

import Common                           (withUnitTests)
import PlantUMLDiagrams                 (renderAll)

import Test.Hspec

spec :: Spec
spec =
  withUnitTests "renderAll" does dir "txt" $ shouldBe . renderAll . read
  where
    does = "generates expected PlantUML code"
    dir = "test/unit/PlantUMLDiagrams"
