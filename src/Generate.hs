module Generate where
import Datatype
import Test.QuickCheck hiding(label,labels)
import Test

randomSD :: Gen UMLStateDiagram
randomSD = do
      let counter = 4
      n <- elements [3 .. 5]
      labels <- shuffle [1..n]
      subs <- mapM (randomInnerSD counter) labels  `suchThat` any checkListInSD
      nm <- elements ["A","B","C"]
      start <- elements (map label subs)
      conns <- vectorOf n (randomConnection (map label subs))
      return (StateDiagram subs 1 nm conns [start])

randomInnerSD :: Int -> Int -> Gen UMLStateDiagram
randomInnerSD c l =
      if c > 0
        then
          do
            let counter = c-1
            frequency [ (8,randomInnerMost l),(1,randomCD counter l),(2,randomSubstateCD counter l)]
      else
          do
            randomInnerMost l

randomInnerMost :: Int -> Gen UMLStateDiagram
randomInnerMost l = do
       nm <- return (last (take l ["A","B","C","D","E"]))
       frequency [(2,return (Joint l)),(1,return (History l Shallow)),(1,return (History l Deep)),(10,return (InnerMostState l nm ""))]

randomCD :: Int -> Int -> Gen UMLStateDiagram
randomCD c l = do
      let counter = c-1
      n <- elements [2 .. 3]
      labels <- shuffle [1..n]
      subs <- mapM (randomSubstateCD counter) labels `suchThat` any checkListInSD
      return (CombineDiagram subs l)

randomSubstateCD :: Int -> Int -> Gen UMLStateDiagram
randomSubstateCD c l = do
      let counter = c-1
      n <- elements [2 .. 3]
      labels <- shuffle [1..n]
      subs <- mapM (randomInnerSD counter) labels `suchThat` any checkListInSD
      nm <- elements ["A","B","C"]
      start <- elements (map label subs)
      conns <- vectorOf n (randomConnection (map label subs))
      return (StateDiagram subs l nm conns [start])

randomConnection :: [Int] -> Gen Connection
randomConnection l = do
      from <- elements l
      to <- elements l `suchThat` (from /=)
      return (Connection [from] [to] "" )
