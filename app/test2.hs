{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text
import Arrows
import Datatype
import Example
import Layout

example :: Wrapper
example = reduceCrossStateCrossing' $ getWrapper $ rearrangeSubstate task85

example2 :: Wrapper
example2 = modifyRightConnection $ assignLength $ assignLayout $ edgeCrossingReduc $
  edgeCrossingReduc $ addDummy $ reduceCrossStateCrossing' $ getWrapper $ rearrangeSubstate slide257

example3 :: Diagram B
example3 =  (circle 0.1 # fc black ===
  ((rect 0.2 (0.3 - 0.2) # lcA transparent) <> arrowAt'
  arrowStyle1 (p2 (0, 0.5 * (- 0.3 + 0.2))) (r2 (0, 0.3 - 0.2)))) # centerXY
  # named ([-1] :: [Int])
main =
  mainWith (drawWrapper' [] (orderFunction task85  ))

