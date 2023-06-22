{-# LANGUAGE NamedFieldPuns #-}

module Checkers.History ( checkHistory ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram(unUML),
  globalise,
  )

import Checkers.Helpers (inCompoundState, notHistory)

checkHistory :: UMLStateDiagram Int -> Maybe String
checkHistory a
  | not (checkEdge False a) =
      Just "History should never be reached from (somewhere, possibly nested) inside their own compound state"
  | not (checkEdge True a) =
      Just "History should only have connections towards something in their own compound state or deeper"
  | otherwise =
      Nothing

checkEdge :: Bool -> UMLStateDiagram Int -> Bool
checkEdge out s =
  if out then
    all (\Connection{pointFrom, pointTo} ->
            inCompoundState pointFrom pointTo)
    $ filter (not.(`notHistory` sub).pointFrom) conn
  else
    all (\Connection{pointFrom, pointTo} ->
            not (inCompoundState pointTo pointFrom))
    $ filter (not.(`notHistory` sub).pointTo) conn
  where
    global = unUML $ globalise s
    sub    = substate global
    conn   = connection global
