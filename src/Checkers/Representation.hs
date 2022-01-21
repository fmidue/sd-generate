{-# LANGUAGE NamedFieldPuns #-}

module Checkers.Representation ( checkRepresentation ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram,
  )

import Checkers.Helpers (getSubstate, lastSecNotCD)

checkRepresentation :: UMLStateDiagram -> Maybe String
checkRepresentation a
  | not (checkOuterMostLayer a) =
      Just "Error: Outermost layer must be 'StateDiagram'"
  | not (checkSubstateCD a) =
      Just "Error: CombineDiagram constructor must contain at least 2 StateDiagram and no other type of constructor"
  | not (checkEmptyConnPoint a) =
      Just "Error: Neither the pointFrom nor the pointTo of a connection is an empty list"
  | not (checkSubC a) =
      Just "Error: Connection Points"
  | not (checkConnFromToRegion a) =
      Just "connections from/to regions themselves"
  | not (checkSubS a) =
      Just "Error: invalid start state"
  | not (checkStartToRegion a) =
      Just "Start to regions themselves"
  | otherwise =
      Nothing

checkOuterMostLayer :: UMLStateDiagram -> Bool
checkOuterMostLayer StateDiagram{} = True
checkOuterMostLayer _ = False

checkSubstateCD :: UMLStateDiagram -> Bool
checkSubstateCD (CombineDiagram a _) = length a > 1 && all checkListInCD a
checkSubstateCD (StateDiagram a _ _ _ _) = all checkSubstateCD a
checkSubstateCD _ = True

checkListInCD :: UMLStateDiagram -> Bool
checkListInCD (StateDiagram a _ _ _ _) = all checkSubstateCD a
checkListInCD _ = False

checkEmptyConnPoint :: UMLStateDiagram -> Bool
checkEmptyConnPoint = all (\cs -> not (any (null . pointFrom) cs) && not (any (null . pointTo) cs))

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

checkConnFromToRegion :: UMLStateDiagram -> Bool
checkConnFromToRegion StateDiagram{substate,connection}  =
                                all ((`lastSecNotCD` substate) . pointFrom) connection
                             && all ((`lastSecNotCD` substate) . pointTo) connection
                             && all checkConnFromToRegion substate
checkConnFromToRegion CombineDiagram {substate} = all checkConnFromToRegion substate
checkConnFromToRegion _ = True

checkSubS :: UMLStateDiagram -> Bool
checkSubS  StateDiagram { substate, startState} = checkStart && all checkSubS substate
                                              where
                                                getLayerList = map label substate
                                                checkStart = isContained1 startState getLayerList substate
checkSubS CombineDiagram {substate} = all checkSubS substate
checkSubS  _ = True

checkStartToRegion :: UMLStateDiagram -> Bool
checkStartToRegion StateDiagram{substate,startState}  =
                                               lastSecNotCD startState substate
                                             && all checkStartToRegion substate
checkStartToRegion CombineDiagram {substate} = all checkStartToRegion substate
checkStartToRegion _ = True
