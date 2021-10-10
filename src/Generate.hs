module Generate (randomSD) where
import Datatype (
  Connection(..),
  HistoryType(..),
  StateDiagram(..),
  UMLStateDiagram,
  )
import Test (checkSemantics)
import Data.Maybe(isNothing)
import Data.List((\\)) 
import Test.QuickCheck hiding(label,labels)

randomSD :: Gen UMLStateDiagram
randomSD = randomSD' `suchThat` (isNothing . checkSemantics)

randomSD' :: Gen UMLStateDiagram
randomSD' = do
      let counter = 3
      n       <- elements [3 .. 4]
      nm      <- elements ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T"]
      labels  <- shuffle  [1..n]
      subSD   <- vectorOf n arbitrary `suchThat` (True `elem`)
      nmUniq  <- vectorOf n (choose (False, True))
      subNms  <- shuffle  (["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T"] \\ [nm])
      let subNm   = chooseName nmUniq  subNms 
      let cond    = zip3 subSD labels subNm
      subs    <- mapM (\x -> randomInnerSD counter x [nm]) cond 
      conns   <- vectorOf n (randomConnection (map label subs))
      start   <- elements (map label subs)
      return (StateDiagram subs 1 nm conns [start]) 

chooseName :: [Bool] -> [String] -> [[String]]
chooseName (x:xs) str = if x == True
                          then 
                            [take 3 str] ++ chooseName xs (drop 3 str) 
                        else 
                          [take 1 str] ++ chooseName xs (drop 1 str)
chooseName [] _ = []

randomInnerSD :: Int -> (Bool,Int,[String]) -> [String]-> Gen UMLStateDiagram
randomInnerSD c (b,l,s) noNm = do
      if b == True
        then
          do
            if length s == 1 
              then 
                do 
                  let nm = head s 
                  frequency [ (10,return (InnerMostState l nm "")),(2,randomSubstateCD c (l,nm) noNm)]
            else 
              do 
               randomCD c l s noNm             
      else
       do
         frequency [(2,return (Joint l)),(1,return (History l Shallow)),(1,return (History l Deep)),(1,return (EndState l))]

randomCD :: Int -> Int -> [String] ->[String] -> Gen UMLStateDiagram
randomCD c l s noNm = do
      n      <- elements [2 .. 3]
      labels <- shuffle [1..n]
      let cond   = zip labels s
      subs   <- mapM (\x -> randomSubstateCD c x noNm) cond
      return (CombineDiagram subs l)

randomSubstateCD :: Int -> (Int,String)-> [String] -> Gen UMLStateDiagram
randomSubstateCD c (l,nm) noNm = do
      let counter = c - 1
          noNms   = noNm ++ [nm]
      n       <- elements [2 .. 3]
      labels  <- shuffle [1..n]
      subSD   <- vectorOf n arbitrary `suchThat` (True `elem`)
      nmUniq  <- vectorOf n (choose (False, True))
      subNms  <- shuffle  (["A","B","C","D","E","F","G","H","I","J","K"] \\ noNms)
      let subNm   = chooseName nmUniq  subNms 
      let cond    = zip3 subSD labels subNm
      subs    <- mapM (\x -> randomInnerSD counter x noNms) cond 
      start   <- elements (map label subs)
      conns   <- vectorOf n (randomConnection (map label subs))
      return (StateDiagram subs l nm conns [start])

randomConnection :: [Int] -> Gen Connection
randomConnection l = do
      from <- elements l
      to   <- elements l `suchThat` (from /=)
      return (Connection [from] [to] "" )


