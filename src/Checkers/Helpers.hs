{-# LANGUAGE NamedFieldPuns #-}

module Checkers.Helpers (
  getAllElem,
  getAllElem1,
  getSameFromTran,
  getSubstates,
  globalStart,
  inCompoundState,
  isNotCD,
  isNotEnd,
  isSDCD,
  lastSecNotCD,
  notHistory,
  notForkOrJoin,
  checkEmptyOutTran,
  checkSameOutTran
  ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram,
  umlStateDiagram,
  unUML,
  globalise
  )

import Data.List (find)

checkSameOutTran :: Eq a => Connection a -> [Connection a] -> Bool
checkSameOutTran a b = length tranSame == 1
                where
                  fromSame  = filter ((pointFrom a ==).pointFrom) b
                  tranSame = filter ((transition a ==).transition) fromSame

checkEmptyOutTran :: Eq a => Connection a -> [Connection a] -> Bool
checkEmptyOutTran a b = length fromSame == 1|| not (null (transition a))
                where
                  fromSame  = filter ((pointFrom a ==).pointFrom) b

getSameFromTran :: [Int] -> StateDiagram n Int [Connection Int] -> [String]
getSameFromTran (x:xs) s@StateDiagram{label}
  = if x == label then getSameFromTran1 xs (umlStateDiagram s) else return []
getSameFromTran (x:xs) CombineDiagram{label, substates}
  = if x == label then concatMap (getSameFromTran xs) substates else return []
getSameFromTran _ _ = []

getSameFromTran1 :: [Int] -> UMLStateDiagram n Int -> [String]
getSameFromTran1 x
  = unUML (\_ _ conn _ -> map transition (filter ((== x) . pointFrom) conn))
    . globalise

inCompoundState :: [Int] -> [Int] -> Bool
inCompoundState a b = init (take (length a) b) == init a

getAllElem :: StateDiagram n Int [Connection Int] -> [[Int]]
getAllElem StateDiagram{substates}
  = map (\x -> [label x]) substates
   ++ concatMap (getAllElem1 []) substates
getAllElem CombineDiagram{substates}
  = map (\x -> [label x]) substates
  ++ concatMap (getAllElem1 []) substates
getAllElem _ = []

getAllElem1 :: [Int] -> StateDiagram n Int [Connection Int] -> [[Int]]
getAllElem1 prepend s@StateDiagram {substates}
  = map (\x -> newPrepend ++ [label x]) substates
    ++ concatMap (getAllElem1 newPrepend) substates
      where
       newPrepend = prepend ++ [label s]
getAllElem1 prepend c@CombineDiagram {substates}
  = map (\x -> newPrepend ++ [label x]) substates
    ++ concatMap (getAllElem1 newPrepend) substates
      where
        newPrepend = prepend ++ [label c]
getAllElem1 _ _ = []

globalStart :: StateDiagram n Int [Connection Int] -> [[Int]]
globalStart StateDiagram{ substates, startState }
 = startState : concatMap (`globalStart1` []) substates
globalStart _ = []

globalStart1 :: StateDiagram n Int [Connection Int] -> [Int] -> [[Int]]
globalStart1 StateDiagram{ substates, startState, label } p
 =  ((p ++ [label]) ++ startState)
    : concatMap (`globalStart1` (p ++ [label])) substates
globalStart1 CombineDiagram{ substates ,label } p
  = concatMap (`globalStart1` (p ++ [label])) substates
globalStart1 _ _ = []

isSDCD :: [Int] -> [StateDiagram n Int [Connection Int]] -> Bool
isSDCD [] _ = False
isSDCD [x] a = any (isSDCD1 x) a
isSDCD (x:xs) a = isSDCD xs (getSubstates x a)

isSDCD1 :: Int -> StateDiagram n Int [Connection Int] -> Bool
isSDCD1 a StateDiagram {label}  = a == label
isSDCD1 a CombineDiagram {label}  = a == label
isSDCD1 _ _ = False

isNotEnd :: [Int] -> [StateDiagram n Int [Connection Int]] -> Bool
isNotEnd [] _ = True
isNotEnd [x] a = all (isNotEnd1 x) a
isNotEnd (x:xs) a = isNotEnd xs (getSubstates x a)

isNotEnd1 :: Int -> StateDiagram n Int [Connection Int] -> Bool
isNotEnd1 a EndState {label}  = a /= label
isNotEnd1 _ _ = True

notHistory :: [Int] -> [StateDiagram n Int [Connection Int]] -> Bool
notHistory [] _ = True
notHistory [x] a = all (isNotHistory x) a
notHistory (x:xs) a = notHistory xs (getSubstates x a)

isNotHistory :: Int -> StateDiagram n Int [Connection Int] -> Bool
isNotHistory a History {label}  = a /= label
isNotHistory _ _ = True

notForkOrJoin :: [Int] -> [StateDiagram n Int [Connection Int]] -> Bool
notForkOrJoin [] _ = True
notForkOrJoin [x] a = all (isNotForkOrJoin x) a
notForkOrJoin (x:xs) a = notForkOrJoin xs (getSubstates x a)

isNotForkOrJoin :: Int -> StateDiagram n Int [Connection Int] -> Bool
isNotForkOrJoin a ForkOrJoin {label} = a /= label
isNotForkOrJoin _ _ = True

lastSecNotCD :: [Int] -> [StateDiagram n Int [Connection Int]]-> Bool
lastSecNotCD [] _ = True
lastSecNotCD [x, _] a = all (isNotCD x) a
lastSecNotCD (x:xs) a = lastSecNotCD xs (getSubstates x a)

isNotCD :: Int -> StateDiagram n Int [Connection Int] -> Bool
isNotCD a CombineDiagram{label} = a /= label
isNotCD _ _ = True

getSubstates :: Int -> [StateDiagram n Int [Connection Int]] -> [StateDiagram n Int [Connection Int]]
getSubstates a xs = maybe [] getSubstates1 (find ((a ==) . label) xs)

getSubstates1 :: StateDiagram n Int [Connection Int] -> [StateDiagram n Int [Connection Int]]
getSubstates1 (StateDiagram a _ _ _ _) = a
getSubstates1 (CombineDiagram a _) = a
getSubstates1 _ = []
