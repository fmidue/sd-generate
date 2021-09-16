import Diagrams.Prelude
import Diagrams.Backend.SVG
import Layout

import Generate

import Test.QuickCheck
import Control.Monad

import Text.Pretty.Simple (pPrint)

main = do
  sds <- sample' randomSD
  forM_ (zip [1..] sds) $
    \(i,sd) ->
      do
        let file = "sample" ++ show i ++ ".svg"
        putStrLn $ "\n" ++ file ++ ":"
        pPrint sd
        renderSVG file (mkWidth 250) (drawDiagram sd)

drawDiagram sd = drawWrapper' [] (orderFunction sd)
