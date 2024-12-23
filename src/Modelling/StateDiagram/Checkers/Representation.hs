{-# OPTIONS_GHC -Wno-error=deprecations #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.StateDiagram.Checkers.Representation ( checkRepresentation ) where

import Modelling.StateDiagram.Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram(unUML'),
  )

import Modelling.StateDiagram.Checkers.Helpers (getSubstates, lastSecNotCD)

checkRepresentation :: UMLStateDiagram n Int -> Maybe String
checkRepresentation a
  | not (checkNegativeLabels $ unUML' a) =
      Just "Negative numbers are not allowed in labels."
  | not (checkSubstatesCD $ unUML' a) =
      Just "Error: CombineDiagram constructor must contain at least 2 StateDiagram and no other type of constructor"
  | not (checkEmptyConnPoint $ unUML' a) =
      Just "Error: Neither the pointFrom nor the pointTo of a connection is an empty list"
  | not (checkSubC $ unUML' a) =
      Just "Error: Connection Points"
  | not (checkConnFromToRegion $ unUML' a) =
      Just "connections from/to regions themselves"
  | not (checkSubS $ unUML' a) =
      Just "Error: invalid start state"
  | not (checkStartToRegion $ unUML' a) =
      Just "Start to regions themselves"
  | otherwise =
      Nothing

checkSubstatesCD :: StateDiagram n Int [Connection Int] -> Bool
checkSubstatesCD (CombineDiagram a _) = length a > 1 && all checkListInCD a
checkSubstatesCD (StateDiagram a _ _ _ _) = all checkSubstatesCD a
checkSubstatesCD _ = True

checkListInCD :: StateDiagram n Int [Connection Int] -> Bool
checkListInCD (StateDiagram a _ _ _ _) = all checkSubstatesCD a
checkListInCD _ = False

checkEmptyConnPoint :: StateDiagram n Int [Connection Int] -> Bool
checkEmptyConnPoint = all (\cs -> not (any (null . pointFrom) cs) && not (any (null . pointTo) cs))

checkSubC :: StateDiagram n Int [Connection Int] -> Bool
checkSubC  StateDiagram { substates, connections } =  checkConnFrom && checkConnTo  && all checkSubC substates
                              where
                                getLayerList = map label substates
                                checkConnFrom = isContained (map pointFrom connections) getLayerList substates
                                checkConnTo = isContained (map pointTo connections) getLayerList substates
checkSubC CombineDiagram { substates } = all checkSubC substates
checkSubC  _ = True

isContained :: [[Int]] -> [Int] -> [StateDiagram n Int [Connection Int]] -> Bool
isContained xs a b = all (\x -> isContained1 x a b) xs

isContained1 :: [Int] -> [Int] -> [StateDiagram n Int [Connection Int]] -> Bool
isContained1 [] _ _ = True
isContained1 (x:xs) a b =  (x `elem` a)  &&
       isContained1 xs (map label (getSubstates x b)) (getSubstates x b)

checkConnFromToRegion :: StateDiagram n Int [Connection Int] -> Bool
checkConnFromToRegion StateDiagram{ substates, connections }  =
                                all ((`lastSecNotCD` substates) . pointFrom) connections
                             && all ((`lastSecNotCD` substates) . pointTo) connections
                             && all checkConnFromToRegion substates
checkConnFromToRegion CombineDiagram { substates } = all checkConnFromToRegion substates
checkConnFromToRegion _ = True

checkSubS :: StateDiagram n Int [Connection Int] -> Bool
checkSubS  StateDiagram { substates, startState } = checkStart && all checkSubS substates
                                              where
                                                getLayerList = map label substates
                                                checkStart = isContained1 startState getLayerList substates
checkSubS CombineDiagram { substates } = all checkSubS substates
checkSubS  _ = True

checkStartToRegion :: StateDiagram n Int [Connection Int] -> Bool
checkStartToRegion StateDiagram{ substates, startState }  =
                                               lastSecNotCD startState substates
                                             && all checkStartToRegion substates
checkStartToRegion CombineDiagram { substates } = all checkStartToRegion substates
checkStartToRegion _ = True

checkNegativeLabels :: StateDiagram n Int [Connection Int] -> Bool
checkNegativeLabels StateDiagram{label,substates} = label >= 0 && all checkNegativeLabels substates
checkNegativeLabels CombineDiagram{label,substates} = label >= 0 && all checkNegativeLabels substates
checkNegativeLabels sd = label sd >= 0
