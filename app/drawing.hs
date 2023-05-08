import Diagrams.Backend.SVG.CmdLine
import Example
import Layout (drawDiagram)
import Example (testFlat1Input)
import Datatype ( localise
                  ,globalise )
import Flatten (transitionLiterals)

main = do
  let sd = testFlat1Input
  mainWith (drawDiagram sd)
