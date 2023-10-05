{-# OPTIONS_GHC -Wno-deprecations #-}
import Diagrams.Backend.SVG.CmdLine
import Layout (drawDiagram)
import Example (--flatCase1
               flatCase2, flatCase1
               )
import Flatten (flatten)
import Distinct (distinctLabels)
import Datatype (rename, unUML')
import Data.List (intercalate)

-- stack run drawing -- --output here.svg -w 640 -h 480
main :: IO ()
main = do
  print "distinctLabels"
  print (distinctLabels $ unUML' flatCase1)
  print "flattened format"
  print (show (flatten flatCase1 ) )
  let sd = flatten flatCase2
  mainWith (drawDiagram (rename (intercalate "_") sd))
