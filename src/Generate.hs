{-# LANGUAGE NamedFieldPuns #-}
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
import Data.List((\\),nub,find) 
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
      conns <- vectorOf n (randomConnection (layerElem ++ globalElem) subs)
      start <- elements (filter (`lastSecNotCD` subs) (layerElem ++ globalElem)) 
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
           frequency [ (10,return (InnerMostState l nm "")),(5,randomSD' counter [3 .. 4] alphabet (l,nm) exclude)]
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

randomConnection :: [[Int]] -> [UMLStateDiagram] -> Gen Connection
randomConnection p sub = do
      let notRegion = filter (`lastSecNotCD` sub) p
          endState  = filter (`isEnd` sub) notRegion 
      from <- elements (notRegion \\ endState)
      to   <- elements notRegion `suchThat` ( /= from)
      tran <- elements ["a","b","c","d","e","f","g","h","i","j","k",""]
      if notHistory from sub 
        then 
          return (Connection from to tran)
        else 
          return (Connection from to "")

isEnd :: [Int] -> [UMLStateDiagram] -> Bool
isEnd [] _ = False
isEnd [x] a = any (isEnd1 x) a
isEnd (x:xs) a = isEnd xs (getSubstate x a)

isEnd1 :: Int -> UMLStateDiagram -> Bool
isEnd1 a EndState{label}  = a == label
isEnd1 _ _ = False

notHistory :: [Int] -> [UMLStateDiagram] -> Bool
notHistory [] _ = True
notHistory [x] a = all (isNotHistory x) a
notHistory (x:xs) a = notHistory xs (getSubstate x a)

isNotHistory :: Int -> UMLStateDiagram -> Bool
isNotHistory a History {label}  = a /= label
isNotHistory _ _ = True

getSubstate :: Int -> [UMLStateDiagram] -> [UMLStateDiagram]
getSubstate a xs = maybe [] getSubstate1 (find ((a ==) . label) xs)

getSubstate1 :: UMLStateDiagram -> [UMLStateDiagram]
getSubstate1 (StateDiagram a _ _ _ _) = a
getSubstate1 (CombineDiagram a _) = a
getSubstate1 _ = []

lastSecNotCD :: [Int] -> [UMLStateDiagram]-> Bool
lastSecNotCD [] _ = True
lastSecNotCD [x, _] a = all (isNotCD x) a
lastSecNotCD (x:xs) a = lastSecNotCD xs (getSubstate x a) 

isNotCD :: Int -> UMLStateDiagram -> Bool
isNotCD a CombineDiagram{label} = a /= label
isNotCD _ _ = True