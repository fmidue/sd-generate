import Diagrams.Backend.SVG.CmdLine
import Layout (drawDiagram)
import Example (testFlat1Input)
import Datatype (globalise )
import Flatten (flatten
   ,groupSameConnections
   ,getConnections
               )

main :: IO ()
main = do
  print "group connections"
  print (show (groupSameConnections(getConnections (globalise testFlat1Input))))
  print "globalized input"
  print (show $ globalise testFlat1Input)
  print "flatten test"
  print (show (flatten testFlat1Input))
  let sd = {- testFlat1Input -} flatten testFlat1Input
  mainWith (drawDiagram sd)
