import Diagrams.Backend.SVG.CmdLine
import Example
import Layout (drawDiagram)
import Example (testFlat1Combine
               ,testFlat1Input
               ,testFlatConReg1
               ,testFlatConReg2)
import Datatype ( localise
                  ,globalise )
import Flatten (flatten
               ,getLabelStr)

main = do
  print (show $ globalise testFlat1Input)
  print (show $ localise testFlat1Input)
  print "flattening test"
  print (show (flatten testFlat1Input))
  let sd = testFlat1Input
  mainWith (drawDiagram sd)

