module Modelling.StateDiagram.Render where

import Diagrams (V2 (V2), absolute, dims, mkHeight, mkWidth)
import Diagrams.Backend.SVG (renderSVG)
import Modelling.StateDiagram.Datatype (UMLStateDiagram)
import Modelling.StateDiagram.Layout (drawDiagram)
import Modelling.StateDiagram.Style (Styling)

drawSDToFile :: FilePath -> (Maybe Double, Maybe Double) -> Styling -> UMLStateDiagram String Int -> IO FilePath
drawSDToFile file mSize style sd = do
  renderSVG file size $ drawDiagram style sd
  pure file
  where
    size = case mSize of
      (Nothing, Nothing) -> absolute
      (Just width, Nothing) -> mkWidth width
      (Nothing, Just height) -> mkHeight height
      (Just width, Just height) -> dims (V2 width height)
