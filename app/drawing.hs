import Diagrams.Backend.SVG.CmdLine
import Layout (drawDiagram)
import Example (--flatCase1
               flatCase2, flatCase1
               )
import Flatten (flatten)

main :: IO ()
main = do
  print "base format"
  print (show flatCase1)
  print "flattened format"
  print (show (flatten flatCase1 ) )
  let sd = flatten flatCase2
  mainWith (drawDiagram sd)
