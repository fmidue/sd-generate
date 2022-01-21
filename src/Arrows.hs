module Arrows where
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

arrowStyle1 :: ArrowOpts (N B)
arrowStyle1 = with & headLength .~ verySmall & arrowHead .~ dart &
  shaftStyle %~ lw thin -- normal connection with arrowhead
arrowStyle2 :: ArrowOpts (N B)
arrowStyle2 = with & headLength .~ verySmall & arrowHead .~ noHead & shaftStyle
  %~ lw thin -- connection without arrowhead
arrowStyle3 :: ArrowOpts (N B)
arrowStyle3 = with & headLength .~ verySmall & shaftStyle %~ lw thin &
  arrowShaft .~ arc xDir (1/6 @@ turn) -- arrow pointing top to bottom
arrowStyle4 :: ArrowOpts (N B)
arrowStyle4 = with & headLength .~ verySmall & arrowHead .~ noHead & shaftStyle %~ lw thin &
  arrowShaft .~ arc xDir (1/6 @@ turn) -- arrow pointing bottom to top

forwardArrowWithHead :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B ->
  Diagram B
forwardArrowWithHead a b = connectPerim' arrowStyle1 a b (0@@turn) (1/2@@turn)
forwardArrowWithoutHead :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B ->
  Diagram B
forwardArrowWithoutHead a b = connectPerim' arrowStyle2 a b (0@@turn)
  (1/2@@turn)
backwardArrowWithHead :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B ->
  Diagram B
backwardArrowWithHead a b = connectPerim' arrowStyle1 a b (1/2@@turn) (0@@turn)
backwardArrowWithoutHead :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B ->
  Diagram B
backwardArrowWithoutHead a b = connectPerim' arrowStyle2 a b (1/2@@turn)
  (0@@turn)
upwardArrowWithHead :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B ->
  Diagram B
upwardArrowWithHead a b = connectPerim' arrowStyle1 a b (1/4@@turn) (3/4@@turn)
upwardArrowWithoutHead :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B ->
  Diagram B
upwardArrowWithoutHead a b = connectPerim' arrowStyle2 a b (1/4@@turn) (3/4@@turn)
downwardArrowWithHead :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B ->
  Diagram B
downwardArrowWithHead a b = connectPerim' arrowStyle1 a b (3/4@@turn) (1/4@@turn)
downwardArrowWithoutHead :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B ->
  Diagram B
downwardArrowWithoutHead a b = connectPerim' arrowStyle2 a b (3/4@@turn) (1/4@@turn)
selfConnect1 :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B ->
  Diagram B
selfConnect1 a b = applyAll [connectPerim' arrowStyle4 a b (1/2@@turn) (0@@turn),
  connectPerim' arrowStyle3 b a (0@@turn) (1/2@@turn)] -- right to left
selfConnect2 :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
selfConnect2 a b = applyAll [connectPerim' arrowStyle3 a b (1/2@@turn) (0@@turn),
  connectPerim' arrowStyle4 b a (0@@turn) (1/2@@turn)] -- left to right
selfConnect3 :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B ->
  Diagram B
selfConnect3 a b = applyAll [connectPerim' arrowStyle4 a b (1/4@@turn) (3/4@@turn),
  connectPerim' arrowStyle3 b a (3/4@@turn) (1/4@@turn)] -- top to bottom
selfConnect4 :: (IsName n1, IsName n2) => n1 -> n2 -> Diagram B -> Diagram B
selfConnect4 a b = applyAll [connectPerim' arrowStyle3 a b (1/4@@turn) (3/4@@turn),
  connectPerim' arrowStyle4 b a (3/4@@turn) (1/4@@turn)] -- bottom to top
