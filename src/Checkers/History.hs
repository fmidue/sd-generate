{-# LANGUAGE NamedFieldPuns #-}

module Checkers.History ( checkHistory ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram,
  globalise,
  )

import Checkers.Helpers (inCompoundState, notHistory)

checkHistory :: UMLStateDiagram -> Maybe String
checkHistory a
  | not (checkInEdge a) =
      Just "History should never be reached from (somewhere, possibly nested) inside their own compound state"
  | not (checkOutEdge a) =
      Just "History should only have connections towards something in their own compound state or deeper"
  | otherwise =
      Nothing

checkInEdge :: UMLStateDiagram -> Bool
checkInEdge s@StateDiagram {} =
     all (\Connection{pointFrom, pointTo} ->
                      not( inCompoundState pointTo pointFrom)) toOnlyHistory
                               where
                                  global = globalise s
                                  sub    = substate global
                                  conn   = connection global
                                  toOnlyHistory = filter (not.(`notHistory` sub).pointTo) conn
checkInEdge _ = True

checkOutEdge :: UMLStateDiagram -> Bool
checkOutEdge s@StateDiagram {} =
     all (\Connection{pointFrom, pointTo} ->
                      inCompoundState pointFrom pointTo) fromOnlyHistory
                               where
                                  global = globalise s
                                  sub    = substate global
                                  conn   = connection global
                                  fromOnlyHistory = filter (not.(`notHistory` sub).pointFrom) conn
checkOutEdge _ = True
