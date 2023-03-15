module Main where

import PlantUMLDiagrams(renderAll)
import Example (positiveExamples)
import Control.Monad (forM_)

import Prelude hiding (writeFile)
import Data.ByteString.Char8 (pack, writeFile)
import Language.PlantUML.Call (drawPlantUMLDiagram, DiagramType(..))

main :: IO ()
main = forM_ positiveExamples $
  \(file, sd) -> do
    let plantUML = pack (renderAll sd)
    writeFile (file ++ ".txt") plantUML
    picture <- drawPlantUMLDiagram SVG plantUML
    writeFile (file ++ ".svg") picture
