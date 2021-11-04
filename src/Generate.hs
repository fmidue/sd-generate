module Generate (randomSD) where
import Datatype (
  Connection(..),
  HistoryType(..),
  StateDiagram(..),
  UMLStateDiagram,
  )
import Test (checkSemantics)
import Helper
import Data.Maybe(isNothing)
import Data.List((\\)) 
import Test.QuickCheck hiding(label,labels)

chooseType :: [Int] -> Gen Int 
chooseType a = do elements a

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

randomSD' :: Int -> [Int] -> [String] -> (Int,String)-> [String] -> Gen UMLStateDiagram
randomSD' c ns alphabet (l,nm) exclude = do
      let counter = c - 1
          noNms   = exclude ++ [nm]
      n      <- elements ns
      labels <- shuffle  [1..n]
      subTypes <- vectorOf n (if counter > 0 then chooseType [1,2,3,4,5,6] else chooseType [1,2,3,6]) `suchThat` checkSubType
      subNms <- shuffle  (alphabet \\ [nm])
      let subNm   = chooseName subTypes subNms 
          cond    = zip3 labels subTypes subNm 
      subs <- mapM (\x -> randomInnerSD counter alphabet x noNms) cond
      let layerElem  = map (\x -> [label x]) subs
          innerElem  = concatMap (getAllElem1 []) subs
          innerElemNoRegions  = filter (`lastSecNotCD` subs) innerElem
      conns <- vectorOf n (randomConnection (layerElem ++ innerElemNoRegions) subs)
      start <- elements (filter (`lastSecNotCD` subs) (layerElem ++ innerElemNoRegions)) 
      return (StateDiagram subs l nm conns start) 

checkSubType :: [Int] -> Bool
checkSubType [] = False
checkSubType a  = length histNum < 2 && length endNum < 2 
                  && ( 2 `elem` a || 3 `elem` a || 4 `elem` a || 5 `elem` a )
                  where
                    histNum = filter (== 1) a
                    endNum = filter (== 2) a

chooseName :: [Int] -> [String] -> [[String]]
chooseName (x:xs) str = if x == 4 
                          then 
                             take 3 str : chooseName xs (drop 3 str) 
                        else 
                           take 1 str : chooseName xs (drop 1 str)
chooseName [] _ = []

randomInnerSD :: Int -> [String] -> (Int,Int,[String]) -> [String]-> Gen UMLStateDiagram
randomInnerSD counter alphabet (l,t,s) exclude = do
  let nm = head s
  case t of 
       1 -> frequency [(1,return (History l Shallow)),(1,return (History l Deep))]
       2 -> return (EndState l)
       3 -> return (InnerMostState l nm "")
       4 -> randomCD counter alphabet l s exclude
       5 -> randomSD' counter [3 .. 4] alphabet (l,nm) exclude
       6 -> return (Joint l)
       _ -> return (InnerMostState l nm "") -- why not exhausted

randomCD :: Int -> [String] -> Int -> [String] ->[String] -> Gen UMLStateDiagram
randomCD counter alphabet l s exclude = do
      n      <- elements [2 .. 3]
      labels <- shuffle [1..n]
      let cond   = zip labels s
      subs   <- mapM (\x -> randomSD' counter [3 .. 4] alphabet x exclude) cond
      return (CombineDiagram subs l)

randomConnection ::[[Int]] -> [UMLStateDiagram] -> Gen Connection
randomConnection points sub = do
      let endState  = filter (`isEnd` sub) points
          --innerElemOnlySDCD = filter (`isSDCD` sub) points
          --innerElemNotSDCD = points \\ innerElemOnlySDCD
          outerHistory = filter (\x -> length x == 1 ) onlyHistory  
                          where  
                           onlyHistory = filter (not.(`notHistory` sub)) points 
          noEndState = points \\ endState 
          noOuterHistory = points \\ outerHistory 
      from <- elements (if length noOuterHistory == 1 then noEndState \\ noOuterHistory else noEndState )
      to   <- elements (noOuterHistory \\ [from]) 
      tran <- elements ["a","b","c","d","e","f","g","h","i","j","k",""]
      if notHistory from sub 
        then
          do
            if notHistory to sub 
              then 
                do  
                  return (Connection from to tran)
            else 
              do
                historyFrom <- elements (filter (not . inCompoundState to) points) 
                return (Connection historyFrom to tran)
      else
        do
          historyTo <- elements (filter (inCompoundState from) noOuterHistory)
          return (Connection from historyTo "")