{-# OPTIONS_GHC -Wno-deprecations #-}

import Diagrams.Backend.SVG.CmdLine
import Layout (drawDiagram)
import Example (flatCase1
               ,test4)
import Flatten (flatten)
import Distinct (distinctLabels)
import Datatype (unUML', rename, umlStateDiagram, label)

-- stack run drawing -- --output here.svg -w 640 -h 480
main :: IO ()
main = do
  print "distinctLabels"
  print ( distinctLabels $ unUML' flatCase1 )
  print "flattened format"
  print ( show (flatten test4 ) )
  print "flattened concatenated format"
  print (show sd)
  mainWith ( drawDiagram sd )
  where
  sd = umlStateDiagram . (\s -> s { label = 999 }) $ unUML' . rename concat $ flatten test4 -- otherwise renderer fails

-- rename concat $ flatten sd
