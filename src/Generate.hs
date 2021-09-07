module Generate where
import Datatype
import Test.QuickCheck

randomSD :: Gen UMLStateDiagram
randomSD = do
  bottom <- elements [False, True]
  if bottom
    then
    do
      l <- elements [1 .. 3]
      elements [Joint l, History l Shallow, InnerMostState l "" ""]
    else
    do
      n <- elements [1 .. 2]
      subs <- vectorOf n randomSD
      l <- elements [1 .. 3]
      nm <- elements ["A","B","C"]
      start <- elements [1 .. n]
      return (StateDiagram subs l nm [] [start])
