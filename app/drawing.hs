import Diagrams.Backend.SVG.CmdLine
import Example
import Layout (drawDiagram)
import Example (testFlat1Combine
               ,testFlatConReg1
               ,testFlatConReg2)
import Datatype ( localise
                  ,globalise )
import Flatten (crossInms
               ,unionTrns
               ,mrgInmsCP)

main = do
  print (unionTrns [testFlatConReg1,testFlatConReg2]) 
  print (crossInms testFlat1Combine)
  print (mrgInmsCP (crossInms testFlat1Combine))
  let sd = testFlat1Input
  mainWith (drawDiagram sd)

