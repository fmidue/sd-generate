{-# LANGUAGE NamedFieldPuns #-}

module Modelling.StateDiagram.Checkers.Uniqueness ( checkUniqueness ) where

import Modelling.StateDiagram.Datatype (
  StateDiagram(..),
  UMLStateDiagram,
  unUML,
  Connection,
  )

import Data.List.Extra

checkUniqueness :: UMLStateDiagram n Int -> Maybe String
checkUniqueness a
  | not (unUML (\_ substates _ _ -> checkRecursively substates) a) =
      Just "Error: Local Uniqueness not fulfilled"
  | otherwise =
      Nothing

checkRecursively :: [StateDiagram n Int [Connection Int]] -> Bool
checkRecursively substates = isUnique (map label substates) && all checkSub substates

checkSub :: StateDiagram n Int [Connection Int] -> Bool
checkSub  StateDiagram {substates} = checkRecursively substates
checkSub  CombineDiagram {substates} = checkRecursively substates
checkSub  _ = True

isUnique :: [Int] -> Bool
isUnique a = not (anySame a)
