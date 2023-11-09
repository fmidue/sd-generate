module Modelling.StateDiagram.PlantUMLDiagramsSpec (spec) where

import Modelling.StateDiagram.Common                           (withUnitTests)
import Modelling.StateDiagram.PlantUMLDiagrams                 (renderAll)
import Modelling.StateDiagram.Datatype                         (umlStateDiagram)
import Modelling.StateDiagram.Style (Styling(..))
import System.IO.Unsafe (unsafePerformIO)

import Test.Hspec

spec :: Spec
spec =
  withUnitTests "renderAll" does dir "txt" $ shouldBe . unsafePerformIO . renderAll Unstyled . umlStateDiagram . read
  where
    does = "generates expected PlantUML code"
    dir = "test/unit/PlantUMLDiagrams"
