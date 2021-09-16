module Generate where
import Datatype
import Test.QuickCheck hiding(label,labels)
import Test

randomSD ::Gen UMLStateDiagram
randomSD = do
  bottom <- elements [False, True]
  if bottom
    then
    do
      n <- elements [2 .. 3]
      labels <- shuffle [1..n]
      subs <- mapM randomSubstateCD labels `suchThat` any checkListInSD
      return (CombineDiagram subs 1)
    else
    do
      n <- elements [1 .. 2]
      labels <- shuffle [1..n]
      subs <- mapM randomInner labels `suchThat` any checkListInSD
      nm <- elements ["A","B","C"]
      start <- elements (map label subs)
      return (StateDiagram subs 1 nm [] [start])


randomSubstateCD ::Int -> Gen UMLStateDiagram
randomSubstateCD l = do
      n <- elements [1 .. 2]
      labels <- shuffle [1..n]
      subs <- mapM randomInner labels `suchThat` any checkListInSD
      nm <- elements ["A","B","C"]
      start <- elements (map label subs)
      return (StateDiagram subs l nm [] [start])


randomInner :: Int -> Gen UMLStateDiagram
randomInner l = do
  bottom <- elements [False, True]
  if bottom
    then
    do
      elements [Joint l, History l Shallow,History l Deep,InnerMostState l "" ""]
    else
    do
      n <- elements [1 .. 2]
      labels <- shuffle [1..n]
      subs <- mapM randomInner labels `suchThat` any checkListInSD
      nm <- elements ["A","B","C"]
      start <- elements (map label subs)
      return (StateDiagram subs l nm [] [start])
