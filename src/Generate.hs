module Generate where
import Datatype
import Test.QuickCheck hiding(label,labels)
import Test

randomSD :: Gen UMLStateDiagram
randomSD = do
      let counter = 3
      n <- elements [3 .. 4]
      labels <- shuffle [1..n]
      subs <- mapM (randomInnerSD counter) labels  `suchThat` any checkListInSD
      nm <- last (take (counter+1) [
            elements ["S4.1","S4.2","S4.3","S4.4"],elements ["S3.1","S3.2","S3.3","S3.4"],
            elements ["S2.1","S2.2","S2.3","S2.4"],elements ["S1.1","S1.2","S1.3","S1.4"] ])
      start <- elements (map label subs)
      conns <- vectorOf n (randomConnection (map label subs))
      return (StateDiagram subs 1 nm conns [start])

randomInnerSD :: Int -> Int -> Gen UMLStateDiagram
randomInnerSD c l =
      if c > 0
        then
          do
            let counter = c
            frequency [ (8,randomInnerMost counter l),(1,randomCD counter l),(2,randomSubstateCD counter l)]
      else
          do
            randomInnerMost c l

randomInnerMost ::Int -> Int -> Gen UMLStateDiagram
randomInnerMost c l = do
       let nm = last (take l ["A","B","C","D","E"])
       if c == 3
         then
           do
             frequency [(2,return (Joint l)),(10,return (InnerMostState l nm ""))]
       else
         do
           frequency [(2,return (Joint l)),(1,return (History l Shallow)),(1,return (History l Deep)),(10,return (InnerMostState l nm ""))]

randomCD :: Int -> Int -> Gen UMLStateDiagram
randomCD c l = do
      let counter = c
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
      nm <- last (take (counter+1) [
            elements ["S4.1","S4.2","S4.3","S4.4"],elements ["S3.1","S3.2","S3.3","S3.4"],
            elements ["S2.1","S2.2","S2.3","S2.4"],elements ["S1.1","S1.2","S1.3","S1.4"] ])
      start <- elements (map label subs)
      conns <- vectorOf n (randomConnection (map label subs))
      return (StateDiagram subs l nm conns [start])

randomConnection :: [Int] -> Gen Connection
randomConnection l = do
      from <- elements l
      to <- elements l `suchThat` (from /=)
      return (Connection [from] [to] "" )
