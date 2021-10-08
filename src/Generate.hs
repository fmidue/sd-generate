module Generate (randomSD) where
import Datatype (
  Connection(..),
  HistoryType(..),
  StateDiagram(..),
  UMLStateDiagram,
  )
import Test (checkSemantics)
import Data.Maybe(isNothing)
import Data.List (deleteFirstsBy)
import Test.QuickCheck hiding(label,labels)

randomSD :: Gen UMLStateDiagram
randomSD = do
      let counter = 3
      n <- elements [3 .. 4]
      labels <- shuffle [1..n]
      nm <- elements ["A","B","C","D","E","F","G","H"]
      subs <- mapM (\x -> randomInnerSD counter x [nm]) labels  `suchThat` any checkListInSD
      start <- elements (map label subs)
      conns <- vectorOf n (randomConnection (map label subs))
      if isNothing (checkSemantics (StateDiagram subs 1 nm conns [start]))
        then
          return (StateDiagram subs 1 nm conns [start]) 
      else
        randomSD

randomInnerSD :: Int -> Int -> [String]-> Gen UMLStateDiagram
randomInnerSD c l noNm = do
      let nm = last (take l (deleteFirstsBy (<=) ["A","B","C","D","E","F","G","H","I"] noNm ))
      if c > 0
        then
          do
            frequency [ (8,randomInnerMost c l nm),(1,randomCD c l noNm),(2,randomSubstateCD c l nm noNm)]                      
      else
       do
         randomInnerMost c l nm

randomInnerMost ::Int -> Int -> String -> Gen UMLStateDiagram
randomInnerMost c l nm = 
       if c == 3
         then
           do
             frequency [(2,return (Joint l)),(10,return (InnerMostState l nm ""))]
       else
         do
           frequency [(2,return (Joint l)),(1,return (History l Shallow)),(1,return (History l Deep)),(10,return (InnerMostState l nm ""))]

randomCD :: Int -> Int ->[String] -> Gen UMLStateDiagram
randomCD c l noNm = do
      n <- elements [2 .. 3]
      labels <- shuffle [1..n]
      nm <- elements ["Z","Y","U","S","X"]
      subs <- mapM (\x -> randomSubstateCD c x nm noNm) labels `suchThat` any checkListInSD
      return (CombineDiagram subs l)

randomSubstateCD :: Int -> Int -> String -> [String] -> Gen UMLStateDiagram
randomSubstateCD c l nm noNm = do
      let counter = c - 1
      n <- elements [2 .. 3]
      labels <- shuffle [1..n]
      let  noNms = noNm ++ [nm]
      subs <- mapM (\x -> randomInnerSD counter x noNms) labels `suchThat` any checkListInSD
      start <- elements (map label subs)
      conns <- vectorOf n (randomConnection (map label subs))
      if isNothing (checkSemantics (StateDiagram subs l nm conns [start]))
        then
          return (StateDiagram subs l nm conns [start])
      else
        randomSubstateCD c l nm noNm

randomConnection :: [Int] -> Gen Connection
randomConnection l = do
      from <- elements l
      to <- elements l `suchThat` (from /=)
      return (Connection [from] [to] "" )

checkListInSD :: UMLStateDiagram -> Bool
checkListInSD Joint {} = False
checkListInSD History {} = False
checkListInSD _ = True
