import Diagrams.Backend.SVG.CmdLine
import Layout (drawDiagram)
import Example (flatCase1,
                testFlat1Input)
import Flatten (flatten)

main :: IO ()
main = do
  print "base format"
  print (show flatCase1)
  print "flatten format"
  print (show (flatten testFlat1Input))
  let sd = {- testFlat1Input -} flatten flatCase1
  mainWith (drawDiagram sd)
