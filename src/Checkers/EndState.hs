{-# LANGUAGE NamedFieldPuns #-}

module Checkers.EndState ( checkEndState ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram,
  )

import Checkers.Helpers (isNotEnd)

checkEndState :: UMLStateDiagram -> Maybe String
checkEndState a
  | not (checkEndOutEdges a) =
      Just "Error: no EndState should have outgoing edges"
  | otherwise =
      Nothing

checkEndOutEdges :: UMLStateDiagram -> Bool
checkEndOutEdges StateDiagram { substate, connection } = all ((`isNotEnd` substate) . pointFrom) connection
                                                         && all checkEndOutEdges substate
checkEndOutEdges CombineDiagram {substate} = all checkEndOutEdges substate
checkEndOutEdges  _ = True
