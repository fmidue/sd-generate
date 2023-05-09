import Diagrams.Backend.SVG.CmdLine
import Layout (drawDiagram)
import Example (testFlat1Input)
import Datatype ( localise
                  ,globalise )
import Flatten (flatten)

main :: IO ()
main = do
  print (show $ globalise testFlat1Input)
  print (show $ localise testFlat1Input)
  print "flattening test"
  print (show (flatten testFlat1Input))
  let sd = testFlat1Input
  mainWith (drawDiagram sd)
