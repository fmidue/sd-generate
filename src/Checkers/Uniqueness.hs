{-# LANGUAGE NamedFieldPuns #-}

module Checkers.Uniqueness ( checkUniqueness ) where

import Datatype (
  StateDiagram(..),
  UMLStateDiagram,
  unUML,
  Connection,
  )

import Data.List.Extra

checkUniqueness :: UMLStateDiagram Int -> Maybe String
checkUniqueness a
  | not (unUML (\_ substate _ _ -> checkRecursively substate) a) =
      Just "Error: Local Uniqueness not fulfilled"
  | otherwise =
      Nothing

checkRecursively :: [StateDiagram Int [Connection Int]] -> Bool
checkRecursively substate = isUnique (map label substate) && all checkSub substate

checkSub :: StateDiagram Int [Connection Int] -> Bool
checkSub  StateDiagram {substate} =  checkRecursively substate
checkSub  CombineDiagram {substate} =  checkRecursively substate
checkSub  _ = True

isUnique :: [Int] -> Bool
isUnique a = not (anySame a)
