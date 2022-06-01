module Main where

import AlloyDiagrams (render)
import Example (positiveExamples)
import Control.Monad (forM_)

main :: IO ()
main = forM_ positiveExamples $
  \(file, sd) -> writeFile (file ++ ".als") $ render sd