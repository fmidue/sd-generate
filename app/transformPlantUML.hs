module Main where

import Modelling.StateDiagram.Style (Styling(..))
import Modelling.StateDiagram.PlantUMLDiagrams(renderAll)
import Modelling.StateDiagram.Example (posPlantUMLExamples)
import Control.Monad (forM_)

import Prelude hiding (writeFile)
import Data.ByteString.Char8 (pack, writeFile)
import Language.PlantUML.Call (drawPlantUMLDiagram, DiagramType(..))

main :: IO ()
main = forM_ posPlantUMLExamples $
  \(file, sd) -> do
    rendered <- renderAll Unstyled sd
    let plantUML = pack rendered
    writeFile (file ++ ".txt") plantUML
    picture <- drawPlantUMLDiagram SVG plantUML
    writeFile (file ++ ".svg") picture
