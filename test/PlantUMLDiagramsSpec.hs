module PlantUMLDiagramsSpec (spec) where

import Common                           (withUnitTests)
import PlantUMLDiagrams                 (renderAll)
import Datatype                         (umlStateDiagram)
import Style (Styling(..))
import System.IO.Unsafe (unsafePerformIO)

import Test.Hspec

spec :: Spec
spec =
  withUnitTests "renderAll" does dir "txt" $ shouldBe . unsafePerformIO . renderAll Unstyled . umlStateDiagram . read
  where
    does = "generates expected PlantUML code"
    dir = "test/unit/PlantUMLDiagrams"
