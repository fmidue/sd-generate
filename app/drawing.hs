{-# OPTIONS_GHC -Wno-deprecations #-}

import Diagrams.Backend.SVG.CmdLine
import Modelling.StateDiagram.Layout (drawDiagram)

import Modelling.StateDiagram.Style (Styling (Unstyled))

-- stack run drawing -- --output here.svg -w 640 -h 480
main :: IO ()
main = do
  putStrLn "Drawing... (Nothing)"
  mainWith ( drawDiagram Unstyled undefined )
  -- renderable x = umlStateDiagram . (\s -> s { label = 999 }) $ unUML' $ rename concat x
