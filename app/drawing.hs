{-# OPTIONS_GHC -Wno-deprecations #-}
import Diagrams.Backend.SVG.CmdLine
import Layout (drawDiagram)
import Example (--flatCase1
               flatCase2, flatCase1
               )
import Flatten (flatten
              -- ,vertexPath
               ,allVertexAddresses
               ,distinctLabels'
               ,vertexByAddress)
import Datatype (rename, unUML')
import Data.List (intercalate)

-- stack run drawing -- --output here.svg -w 640 -h 480
main :: IO ()
main = do
  print "all vertex addresses"
  print (reverse $ allVertexAddresses $ unUML' flatCase1)
  -- print "pathToVertex"
  -- print (vertexPath [4,1] $ unUML' flatCase1)
  print "all vertices by address"
  print (map (\x -> show x ++ " " ++ show (vertexByAddress x $ unUML' flatCase1)) (allVertexAddresses $ unUML' flatCase1 ))
  print "distinctLabels'"
  print (distinctLabels' $ unUML' flatCase1)
  print "flattened format"
  print (show (flatten flatCase1 ) )
  let sd = flatten flatCase2
  mainWith (drawDiagram (rename (intercalate "_") sd))
