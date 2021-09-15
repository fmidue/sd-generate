module Generate where
import Datatype
import Test.QuickCheck hiding(label)
import Test

randomSD :: Gen UMLStateDiagram
randomSD = do
  bottom <- elements [False, True]
  if bottom
    then
    do
      n <- elements [2 .. 3]
      subs <- vectorOf n randomSubstateCD
      l <- elements [1 .. 3]
      if any checkListInSD subs && isUnique (map label subs)
        then
        do
          return (CombineDiagram subs l)
        else
        do
          randomSD
    else
    do
      n <- elements [1 .. 2]
      subs <- vectorOf n randomInner
      l <- elements [1 .. 3]
      nm <- elements ["A","B","C"]
      start <- elements (map label subs)
      if any checkListInSD subs && isUnique (map label subs)
        then
        do
          return (StateDiagram subs l nm [] [start])
        else
        do
          randomSD

randomSubstateCD :: Gen UMLStateDiagram
randomSubstateCD = do
      n <- elements [1 .. 2]
      subs <- vectorOf n randomInner
      l <- elements [1 .. 3]
      nm <- elements ["A","B","C"]
      start <- elements (map label subs)
      if any checkListInSD subs && isUnique (map label subs )
        then
        do
          return (StateDiagram subs l nm [] [start])
        else
        do
          randomSubstateCD

randomInner :: Gen UMLStateDiagram
randomInner = do
  bottom <- elements [False, True]
  if bottom
    then
    do
      l <- elements [1 .. 4]
      elements [Joint l, History l Shallow,InnerMostState l "" "",History l Deep]
    else
    do
      n <- elements [1 .. 2]
      subs <- vectorOf n randomInner
      l <- elements [1 .. 3]
      nm <- elements ["A","B","C"]
      start <- elements (map label subs)
      if any checkListInSD subs && isUnique (map label subs)
        then
        do
          return (StateDiagram subs l nm [] [start])
        else
        do
          randomInner
