import Diagrams.Prelude
import Diagrams.Backend.SVG
import Layout (drawDiagram)

import Generate

import Test.QuickCheck
import Control.Monad

import Text.Pretty.Simple (pPrint)

main = do
  sds <- sample' randomSD
  forM_ (zip [1..] sds) $
    \(i,(sd,n)) ->
      do
        let file = "sample" ++ show i ++ ".svg"
        putStrLn $ "\nDiscarded " ++ show n ++ " attempts, then got " ++ file ++ ":"
        pPrint sd
        renderSVG file (mkWidth 250) (drawDiagram sd)
