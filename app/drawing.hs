{-# OPTIONS_GHC -Wno-deprecations #-}

import Diagrams.Backend.SVG.CmdLine
import Layout (drawDiagram)
import Example (noHistoryTest4)
import Flatten (flatten
               ,flatten')

import Datatype (unUML'
                ,rename
                ,umlStateDiagram
                ,label)


-- stack run drawing -- --output here.svg -w 640 -h 480
main :: IO ()
main = do
  print "step1"
  print sd
  print "step2"
  print sd'
  print "step3"
  print sd''
  print "step4"
  print sd'''
  mainWith ( drawDiagram $ renderable sd''' )
  where
  sd = flatten noHistoryTest4
  sd' = flatten' sd
  sd'' = flatten' sd'
  sd''' = flatten' sd''
  renderable x = umlStateDiagram . (\s -> s { label = 999 }) $ unUML' $ rename concat x
