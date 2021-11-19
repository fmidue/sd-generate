{-# LANGUAGE NamedFieldPuns #-}

module Checkers.Uniqueness ( checkUniqueness ) where

import Datatype (
  StateDiagram(..),
  UMLStateDiagram,
  )

import Data.List.Extra

checkUniqueness :: UMLStateDiagram -> Maybe String
checkUniqueness a
  | not (checkSub a) =
      Just "Error: Local Uniqueness not fullfilled"
  | otherwise =
      Nothing

checkSub :: UMLStateDiagram -> Bool
checkSub  StateDiagram {substate} =  isUnique (map label substate ) && all checkSub substate
checkSub  CombineDiagram {substate} =  isUnique (map label substate ) && all checkSub substate
checkSub  _ = True

isUnique :: [Int] -> Bool
isUnique a = not (anySame a)
