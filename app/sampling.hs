import Diagrams.Prelude
import Diagrams.Backend.SVG
import Modelling.StateDiagram.Layout (drawDiagram)

import Modelling.StateDiagram.Generate
import Modelling.StateDiagram.Checkers (checkDrawability)

import Test.QuickCheck
import Control.Monad
import Data.Maybe

import Text.Pretty.Simple (pPrint)

import Modelling.StateDiagram.Datatype.ClassInstances ()
import Modelling.StateDiagram.Style (Styling (Unstyled))

main :: IO ()
main = do
  sds <- sample' randomSD
  forM_ (zip [1 :: Int ..] sds) $
    \(i,(sd,n)) ->
      do
        let file = "sample" ++ show i ++ ".svg"
        putStrLn $ "\nDiscarded " ++ show n ++ " attempts, then got " ++ file ++ ":"
        pPrint sd
        when (isNothing $ checkDrawability sd)
          $ renderSVG file (mkWidth 250) (drawDiagram Unstyled sd)
