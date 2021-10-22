module Generate (randomSD) where
import Datatype (
  Connection(..),
  HistoryType(..),
  StateDiagram(..),
  UMLStateDiagram,
  hoistOutwards
  )
import Test (checkSemantics)
import Data.Maybe(isNothing)
import Data.List((\\),nub) 
import Test.QuickCheck hiding(label,labels)

suchThatWhileCounting :: Gen a -> (a -> Bool) -> Gen (a, Int)
suchThatWhileCounting gen p = tryWith 0
  where
    tryWith i = do
      a <- gen
      if p a then return (a, i) else tryWith (i + 1)

randomSD :: Gen (UMLStateDiagram, Int)
randomSD = do
      let alphabet = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y"]
      nm <- elements alphabet
      randomSD' 4 [3 .. 4] alphabet (1,nm) [] `suchThatWhileCounting` (isNothing . checkSemantics)

randomSD' :: Int -> [Int] -> [String] -> (Int,String)-> [String] ->Gen UMLStateDiagram
randomSD' c ns alphabet (l,nm) exclude = do
      let counter = c - 1
          noNms   = exclude ++ [nm]
      n      <- elements ns
      labels <- shuffle  [1..n]
      subSD  <- vectorOf n arbitrary `suchThat` (True `elem`)
      nmUniq <- vectorOf n (choose (True, False))
      subNms <- shuffle  (alphabet \\ [nm])
      let subNm   = chooseName nmUniq subNms 
          cond    = zip3 subSD labels subNm
      subs <- mapM (\x -> randomInnerSD counter alphabet x noNms) cond
      let globalConn = hoistOutwards (StateDiagram subs l nm [] [])
          globalElem = nub (map pointTo globalConn ++ map pointFrom globalConn)
          layerElem  = map (\x -> [label x]) subs 
      conns <- vectorOf n (randomConnection (layerElem ++ globalElem))
      start <- elements (layerElem ++ globalElem)
      return (StateDiagram subs l nm conns start) 

chooseName :: [Bool] -> [String] -> [[String]]
chooseName (x:xs) str = if x 
                          then 
                             take 3 str : chooseName xs (drop 3 str) 
                        else 
                           take 1 str : chooseName xs (drop 1 str)
chooseName [] _ = []

randomInnerSD :: Int -> [String] -> (Bool,Int,[String]) -> [String]-> Gen UMLStateDiagram
randomInnerSD counter alphabet (b,l,s) exclude = do
  let nm = head s
  if counter > 0 
   then
    do
     if b 
      then
       do
        if length s == 1 
         then 
          do 
           frequency [ (1,return (InnerMostState l nm "")),(90,randomSD' counter [3 .. 4] alphabet (l,nm) exclude)]
        else 
          do 
           randomCD counter alphabet l s exclude             
     else
      do
       frequency [(2,return (Joint l)),(1,return (History l Shallow)),(1,return (History l Deep)),(1,return (EndState l))]
  else
   do 
    frequency [(10,return (InnerMostState l nm "")),(2,return (Joint l)),(1,return (History l Shallow)),(1,return (History l Deep)),(1,return (EndState l))] 

randomCD :: Int -> [String] -> Int -> [String] ->[String] -> Gen UMLStateDiagram
randomCD counter alphabet l s exclude = do
      n      <- elements [2 .. 3]
      labels <- shuffle [1..n]
      let cond   = zip labels s
      subs   <- mapM (\x -> randomSD' counter [3 .. 4] alphabet x exclude) cond
      return (CombineDiagram subs l)

randomConnection :: [[Int]] -> Gen Connection
randomConnection l = do
      from <- elements l
      to   <- elements l `suchThat` (from /=)
      return (Connection from to "" )
