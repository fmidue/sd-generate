{-# LANGUAGE NamedFieldPuns #-}

module Checkers.Representation ( checkRepresentation ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram(unUML),
  )

import Checkers.Helpers (getSubstate, lastSecNotCD)

checkRepresentation :: UMLStateDiagram Int -> Maybe String
checkRepresentation a
  | not (checkSubstateCD $ unUML a) =
      Just "Error: CombineDiagram constructor must contain at least 2 StateDiagram and no other type of constructor"
  | not (checkEmptyConnPoint $ unUML a) =
      Just "Error: Neither the pointFrom nor the pointTo of a connection is an empty list"
  | not (checkSubC $ unUML a) =
      Just "Error: Connection Points"
  | not (checkConnFromToRegion $ unUML a) =
      Just "connections from/to regions themselves"
  | not (checkSubS $ unUML a) =
      Just "Error: invalid start state"
  | not (checkStartToRegion $ unUML a) =
      Just "Start to regions themselves"
  | otherwise =
      Nothing

checkSubstateCD :: StateDiagram Int [Connection Int] -> Bool
checkSubstateCD (CombineDiagram a _) = length a > 1 && all checkListInCD a
checkSubstateCD (StateDiagram a _ _ _ _) = all checkSubstateCD a
checkSubstateCD _ = True

checkListInCD :: StateDiagram Int [Connection Int] -> Bool
checkListInCD (StateDiagram a _ _ _ _) = all checkSubstateCD a
checkListInCD _ = False

checkEmptyConnPoint :: StateDiagram Int [Connection Int] -> Bool
checkEmptyConnPoint = all (\cs -> not (any (null . pointFrom) cs) && not (any (null . pointTo) cs))

checkSubC :: StateDiagram Int [Connection Int] -> Bool
checkSubC  StateDiagram { substate, connection } =  checkConnFrom && checkConnTo  && all checkSubC substate
                              where
                                getLayerList = map label substate
                                checkConnFrom = isContained (map pointFrom connection) getLayerList  substate
                                checkConnTo = isContained (map pointTo connection ) getLayerList substate
checkSubC CombineDiagram {substate} = all checkSubC substate
checkSubC  _ = True

isContained :: [[Int]] -> [Int] -> [StateDiagram Int [Connection Int]] -> Bool
isContained xs a b = all (\x -> isContained1 x a b) xs

isContained1 :: [Int] -> [Int] -> [StateDiagram Int [Connection Int]] -> Bool
isContained1 [] _ _ = True
isContained1 (x:xs) a b =  (x `elem` a)  &&
       isContained1 xs (map label (getSubstate x b)) (getSubstate x b)

checkConnFromToRegion :: StateDiagram Int [Connection Int] -> Bool
checkConnFromToRegion StateDiagram{substate,connection}  =
                                all ((`lastSecNotCD` substate) . pointFrom) connection
                             && all ((`lastSecNotCD` substate) . pointTo) connection
                             && all checkConnFromToRegion substate
checkConnFromToRegion CombineDiagram {substate} = all checkConnFromToRegion substate
checkConnFromToRegion _ = True

checkSubS :: StateDiagram Int [Connection Int] -> Bool
checkSubS  StateDiagram { substate, startState} = checkStart && all checkSubS substate
                                              where
                                                getLayerList = map label substate
                                                checkStart = isContained1 startState getLayerList substate
checkSubS CombineDiagram {substate} = all checkSubS substate
checkSubS  _ = True

checkStartToRegion :: StateDiagram Int [Connection Int] -> Bool
checkStartToRegion StateDiagram{substate,startState}  =
                                               lastSecNotCD startState substate
                                             && all checkStartToRegion substate
checkStartToRegion CombineDiagram {substate} = all checkStartToRegion substate
checkStartToRegion _ = True
