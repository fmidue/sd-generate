{-# LANGUAGE NamedFieldPuns #-}
module Test
  ( checkSemantics
  , checkStartState
  , checkConnection
  , checkNameUniqueness
  , checkUniqueness
  , checkStructure
  , checkWrapper
  ) where

import Datatype
import Layout
import Data.List.Extra

--check semantics
checkSemantics :: UMLStateDiagram -> Maybe String
checkSemantics a
  | not(checkOutermostHistory a) = Just "Error: History does not make sense in the outermost stateDiagram "
  | not(checkEndOutEdges a) = Just "Error: no EndState should have outgoing edges"
  | otherwise = Nothing

                --checkOutermostHistory
checkOutermostHistory :: UMLStateDiagram -> Bool
checkOutermostHistory (StateDiagram a _ _ _ _) = all isHistoryNotInSD a
checkOutermostHistory _ = True

isHistoryNotInSD :: UMLStateDiagram -> Bool
isHistoryNotInSD History {} = False
isHistoryNotInSD _ = True

                --checkEndOutEdges
checkEndOutEdges :: UMLStateDiagram -> Bool
checkEndOutEdges StateDiagram { substate, connection } = all ((`isConnFromNotEnd` substate) . pointFrom) connection
                                                         && all checkEndOutEdges substate
checkEndOutEdges CombineDiagram {substate} = all checkEndOutEdges substate
checkEndOutEdges  _ = True

isConnFromNotEnd :: [Int] -> [UMLStateDiagram] -> Bool
isConnFromNotEnd [] _ = True
isConnFromNotEnd [x] a = all (isNotEnd x) a
isConnFromNotEnd (x:xs) a = isConnFromNotEnd xs (getSubstate x a)

isNotEnd :: Int -> UMLStateDiagram -> Bool
isNotEnd a EndState {label}  = a /= label
isNotEnd _ _ = True

-- check if  start state is valid
checkStartState :: UMLStateDiagram -> Maybe String
checkStartState a
  | not(checkSubS a) = Just "Error: invalid start state "
  | otherwise = Nothing

checkSubS :: UMLStateDiagram -> Bool
checkSubS  StateDiagram { substate, startState} = checkStart && all checkSubS substate
                                              where
                                                getLayerList = map label substate
                                                checkStart = isContained1 startState getLayerList substate
checkSubS CombineDiagram {substate} = all checkSubS substate
checkSubS  _ = True

--check Connection Points
checkConnection :: UMLStateDiagram -> Maybe String
checkConnection a
  | not(checkSubC a) = Just "Error:  Connection Points"
  | otherwise = Nothing

checkSubC :: UMLStateDiagram -> Bool
checkSubC  StateDiagram { substate, connection } =  checkConnFrom && checkConnTo  && all checkSubC substate
                              where
                                getLayerList = map label substate
                                checkConnFrom = isContained (map pointFrom connection) getLayerList  substate
                                checkConnTo = isContained (map pointTo connection ) getLayerList substate
checkSubC CombineDiagram {substate} = all checkSubC substate
checkSubC  _ = True

isContained :: [[Int]] -> [Int] -> [UMLStateDiagram] -> Bool
isContained xs a b = all (\x -> isContained1 x a b) xs

isContained1 :: [Int] -> [Int] -> [UMLStateDiagram] -> Bool
isContained1 [] _ _ = True
isContained1 (x:xs) a b =  (x `elem` a)  &&
       isContained1 xs (map label (getSubstate x b)) (getSubstate x b)

getSubstate :: Int -> [UMLStateDiagram] -> [UMLStateDiagram]
getSubstate a xs = maybe [] getSubstate1 (find ((a ==) . label) xs)

getSubstate1 :: UMLStateDiagram -> [UMLStateDiagram]
getSubstate1 (StateDiagram a _ _ _ _) = a
getSubstate1 (CombineDiagram a _) = a
getSubstate1 _ = []

--check name uniqueness
checkNameUniqueness :: UMLStateDiagram -> Maybe String
checkNameUniqueness a
  | not(checkSubNameUniq a) = Just "Error: At the same layer name Uniqueness is not fullfilled "
  | not(checkSDNameUniq a) = Just ("Error: In each StateDiagram, the name (if not empty) should be different"
    ++"from all names found arbitrarily deep inside the substates.")
  | otherwise = Nothing

checkSDNameUniq :: UMLStateDiagram -> Bool
checkSDNameUniq StateDiagram {substate,name} = name `notElem` getLayerName substate
                                              && all (checkDeeperUniq name) substate
                                              && all checkSDNameUniq substate
checkSDNameUniq CombineDiagram {substate} = all checkSDNameUniq substate
checkSDNameUniq  _ = True

checkDeeperUniq :: String -> UMLStateDiagram -> Bool
checkDeeperUniq a StateDiagram {substate} = a `notElem` getLayerName substate
                                              && all (checkDeeperUniq a) substate
checkDeeperUniq a CombineDiagram {substate} = all (checkDeeperUniq a) substate
checkDeeperUniq _ _ = True

checkSubNameUniq :: UMLStateDiagram -> Bool
checkSubNameUniq StateDiagram {substate} =  not (anySame (getLayerName substate))
                                            && all checkSubNameUniq  substate
checkSubNameUniq  CombineDiagram {substate} = not (anySame (getLayerName substate))
                                              && all checkSubNameUniq  substate
checkSubNameUniq  _ = True

getLayerName :: [UMLStateDiagram] -> [String]
getLayerName a = filter (not . null) (concatMap getName a)

getName :: UMLStateDiagram -> [String]
getName StateDiagram{name} = [name]
getName InnerMostState{name} = [name]
getName CombineDiagram{substate} = concatMap getName substate
getName _ = []

--check local uniqueness
checkUniqueness :: UMLStateDiagram -> Maybe String
checkUniqueness a
  | not(checkSub a) = Just "Error: Local Uniqueness not fullfilled "
  | otherwise = Nothing

checkSub :: UMLStateDiagram -> Bool
checkSub  StateDiagram {substate} =  isUnique (map label substate ) && all checkSub substate
checkSub  CombineDiagram {substate} =  isUnique (map label substate ) && all checkSub substate
checkSub  _ = True

isUnique :: [Int] -> Bool
isUnique a = not (anySame a)

--checkStructure
checkStructure :: UMLStateDiagram -> Maybe String
checkStructure a
  | not(checkOuterMostLayer a) = Just ("Error: Outermost layer must be 'StateD"
    ++ "iagram")
  | not(checkSubstateSD a) = Just ("Error: Substate of StateDiagram constructo"
    ++ "r cannot be empty or just History/Joint")
  | not(checkSubstateCD a) = Just ("Error: CombineDiagram constructor must con"
    ++ "tain at least 2 StateDiagram and no other type of constructor")
  | not(checkHistOutTransition a) = Just ("Error: Outgoing edges from a history"
    ++ "node always have the empty transition")
  | not(checkSameConnection a) = Just ("Error: No two connections are allowed leaving"
    ++ "the same source and and having the same label.")
  | otherwise = Nothing

                --checkOuterMostLayer
checkOuterMostLayer :: UMLStateDiagram -> Bool
checkOuterMostLayer StateDiagram{} = True
checkOuterMostLayer _ = False

                --checkSubstateSD
checkSubstateSD :: UMLStateDiagram -> Bool
checkSubstateSD (CombineDiagram a _) = all checkSubstateSD a
checkSubstateSD (StateDiagram a _ _ _ _) = any checkListInSD a && all checkSubstateSD a
checkSubstateSD _ = True

checkListInSD :: UMLStateDiagram -> Bool
checkListInSD Joint {} = False
checkListInSD History {} = False
checkListInSD _ = True

                --checkSubstateCD
checkSubstateCD :: UMLStateDiagram -> Bool
checkSubstateCD (CombineDiagram a _) = length a > 1 && all checkListInCD a
checkSubstateCD (StateDiagram a _ _ _ _) = all checkSubstateCD a
checkSubstateCD _ = True

checkListInCD :: UMLStateDiagram -> Bool
checkListInCD (StateDiagram a _ _ _ _) = all checkSubstateCD a
checkListInCD _ = False

                 --checkHistOutTransitions
checkHistOutTransition :: UMLStateDiagram -> Bool
checkHistOutTransition StateDiagram { substate, connection } = all (`checkHistConnTransition` substate) connection
                                                               && all checkHistOutTransition substate
checkHistOutTransition CombineDiagram {substate} = all checkHistOutTransition substate
checkHistOutTransition  _ = True

checkHistConnTransition :: Connection -> [UMLStateDiagram] -> Bool
checkHistConnTransition Connection { pointFrom,transition } a = isConnFromNotHistory pointFrom a || null transition

isConnFromNotHistory :: [Int] -> [UMLStateDiagram] -> Bool
isConnFromNotHistory [] _ = True
isConnFromNotHistory [x] a = all (isNotHistory x) a
isConnFromNotHistory (x:xs) a = isConnFromNotHistory xs (getSubstate x a)

isNotHistory :: Int -> UMLStateDiagram -> Bool
isNotHistory a History {label}  = a /= label
isNotHistory _ _ = True

                --checkSameConnection
checkSameConnection :: UMLStateDiagram -> Bool
checkSameConnection a  = not (anySame (filter (not . null . snd) (getConnectionList a [])))

getConnectionList :: UMLStateDiagram -> [Int] -> [([Int],String)]
getConnectionList StateDiagram { substate, connection ,label} a = map (\x -> getConnection x (a ++ [label])) connection
                                                       ++ concatMap (\ x -> getConnectionList x (a ++ [label])) substate
getConnectionList CombineDiagram {substate,label} a = concatMap (\ x -> getConnectionList x (a ++ [label])) substate
getConnectionList  _ _ =[]

getConnection :: Connection -> [Int] -> ([Int],String)
getConnection Connection{pointFrom,transition} a =  (from,tran)
                                              where from = a ++ pointFrom
                                                    tran  = transition
