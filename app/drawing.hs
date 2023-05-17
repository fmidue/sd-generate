import Diagrams.Backend.SVG.CmdLine
import Layout (drawDiagram)
import Example (testFlat1Input)
import Datatype (globalise )
import Flatten (flatten
   ,groupSameConnections
   ,getConnections
   ,FlatUMLStateDiagram
   ,inFlatForm
               )

main :: IO ()
main = do
  print "preparation: inFlatForm"
  print (show ((inFlatForm testFlat1Input)::FlatUMLStateDiagram)) -- yes we need that bracket hlint
  print "group connections"
  print (show (groupSameConnections(getConnections (globalise testFlat1Input))))
  print "globalized input"
  print (show $ globalise testFlat1Input)
  print "flatten test"
  let sd = {- testFlat1Input -} flatten testFlat1Input
  mainWith (drawDiagram sd)
