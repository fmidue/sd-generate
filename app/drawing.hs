import Diagrams.Backend.SVG.CmdLine
import Layout (drawDiagram)
import Example (testFlat1Input)
import Datatype (globalise )
import Flatten (flatten
   ,removeJointConnection
               )

main :: IO ()
main = do
  print "remove joint"
  print (show (removeJointConnection (globalise testFlat1Input)))
  print "flatten test"
  let sd = {- testFlat1Input -} flatten testFlat1Input
  mainWith (drawDiagram sd)
