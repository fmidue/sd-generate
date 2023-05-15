import Diagrams.Backend.SVG.CmdLine
import Layout (drawDiagram)
import Example (testFlat1Input)
import Datatype ( localise
                  ,globalise )
import Flatten (flatten,
                mergeStates,
                truncateUpperLabels)

main :: IO ()
main = do
  print (show $ globalise testFlat1Input)
  print (show $ localise testFlat1Input)
  print "flatten test"
  print (show (truncateUpperLabels( mergeStates (flatten testFlat1Input))))
  let sd = {- testFlat1Input -} testFlat1Input
  mainWith (drawDiagram sd)
