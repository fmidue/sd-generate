import Diagrams.Backend.SVG.CmdLine
import Example
import Layout (drawDiagram)

main = do
  let sd = verySmall
  mainWith (drawDiagram sd)
