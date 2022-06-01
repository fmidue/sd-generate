module Main where

import PlantUMLDiagrams(renderAll)
import Example (positiveExamples)
import Control.Monad (forM_)

main :: IO ()
main = forM_ positiveExamples $
  \(file, sd) -> writeFile (file ++ ".txt") $ renderAll sd