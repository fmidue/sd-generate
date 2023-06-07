import Diagrams.Backend.SVG.CmdLine
import Layout (drawDiagram)
import Example (--flatCase1
               flatCase2
               )
import Flatten (flatten)

main :: IO ()
main = do
  print "base format"
  print (show flatCase2)
  print "flatten format"
  print (show (flatten flatCase2))
  let sd = {- testFlat1Input -} flatten flatCase2
  mainWith (drawDiagram sd)
