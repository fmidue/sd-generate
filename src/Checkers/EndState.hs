{-# LANGUAGE NamedFieldPuns #-}

module Checkers.EndState ( checkEndState ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram(unUML),
  )

import Checkers.Helpers (isNotEnd)

checkEndState :: UMLStateDiagram Int -> Maybe String
checkEndState a
  | not (checkEndOutEdges $ unUML a) =
      Just "Error: no EndState should have outgoing edges"
  | otherwise =
      Nothing

checkEndOutEdges :: StateDiagram Int [Connection Int] -> Bool
checkEndOutEdges StateDiagram { substate, connection } = all ((`isNotEnd` substate) . pointFrom) connection
                                                         && all checkEndOutEdges substate
checkEndOutEdges CombineDiagram {substate} = all checkEndOutEdges substate
checkEndOutEdges  _ = True
