{-# LANGUAGE NamedFieldPuns #-}

module Checkers.Uniqueness ( checkUniqueness ) where

import Datatype (
  StateDiagram(..),
  UMLStateDiagram(unUML),
  Connection,
  )

import Data.List.Extra

checkUniqueness :: UMLStateDiagram Int -> Maybe String
checkUniqueness a
  | not (checkSub $ unUML a) =
      Just "Error: Local Uniqueness not fulfilled"
  | otherwise =
      Nothing

checkSub :: StateDiagram Int [Connection Int] -> Bool
checkSub  StateDiagram {substate} =  isUnique (map label substate ) && all checkSub substate
checkSub  CombineDiagram {substate} =  isUnique (map label substate ) && all checkSub substate
checkSub  _ = True

isUnique :: [Int] -> Bool
isUnique a = not (anySame a)
