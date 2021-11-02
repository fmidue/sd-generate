{-# LANGUAGE NamedFieldPuns #-}
module Helper where
import Datatype (
  StateDiagram(..),
  UMLStateDiagram,
  )
import Data.List(find) 

inCompoundState :: [Int] -> [Int] -> Bool 
inCompoundState a b = init (take (length a) b) == init a 

getAllElem :: UMLStateDiagram -> [[Int]]
getAllElem StateDiagram{substate} 
  = map (\x -> [label x]) substate  
   ++ concatMap (getAllElem1 []) substate
getAllElem CombineDiagram{substate} 
  = concatMap (getAllElem1 []) substate
getAllElem _ = []

getAllElem1 :: [Int] -> UMLStateDiagram -> [[Int]]
getAllElem1 prepend s@StateDiagram {substate} 
  = map (\x -> newPrepend ++ [label x]) substate
    ++ concatMap (getAllElem1 newPrepend) substate
      where 
       newPrepend = prepend ++ [label s]
getAllElem1 prepend c@CombineDiagram {substate} 
  = concatMap (getAllElem1 newPrepend) substate 
      where
        newPrepend = prepend ++ [label c]
getAllElem1 _ _ = []

isSDCD :: [Int] -> [UMLStateDiagram] -> Bool
isSDCD [] _ = False
isSDCD [x] a = any (isSDCD1 x) a
isSDCD (x:xs) a = isSDCD xs (getSubstate x a)

isSDCD1 :: Int -> UMLStateDiagram -> Bool
isSDCD1 a StateDiagram {label}  = a == label
isSDCD1 a CombineDiagram {label}  = a == label
isSDCD1 _ _ = False

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

lastSecNotCD :: [Int] -> [UMLStateDiagram]-> Bool
lastSecNotCD [] _ = True
lastSecNotCD [x, _] a = all (isNotCD x) a
lastSecNotCD (x:xs) a = lastSecNotCD xs (getSubstate x a) 

isNotCD :: Int -> UMLStateDiagram -> Bool
isNotCD a CombineDiagram{label} = a /= label
isNotCD _ _ = True

getSubstate :: Int -> [UMLStateDiagram] -> [UMLStateDiagram]
getSubstate a xs = maybe [] getSubstate1 (find ((a ==) . label) xs)

getSubstate1 :: UMLStateDiagram -> [UMLStateDiagram]
getSubstate1 (StateDiagram a _ _ _ _) = a
getSubstate1 (CombineDiagram a _) = a
getSubstate1 _ = []