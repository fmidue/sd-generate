{-# OPTIONS_GHC -Wno-error=deprecations #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.StateDiagram.Checkers.EndState ( checkEndState ) where

import Modelling.StateDiagram.Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram(unUML'),
  )

import Modelling.StateDiagram.Checkers.Helpers (isNotEnd)

checkEndState :: UMLStateDiagram n Int -> Maybe String
checkEndState a
  | not (checkEndOutEdges $ unUML' a) =
      Just "Error: no EndState should have outgoing edges"
  | otherwise =
      Nothing

checkEndOutEdges :: StateDiagram n Int [Connection Int] -> Bool
checkEndOutEdges StateDiagram { substates, connections } = all ((`isNotEnd` substates) . pointFrom) connections
                                                         && all checkEndOutEdges substates
checkEndOutEdges CombineDiagram { substates } = all checkEndOutEdges substates
checkEndOutEdges  _ = True
