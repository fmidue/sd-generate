{-# LANGUAGE NamedFieldPuns #-}

module Modelling.StateDiagram.Checkers.History ( checkHistory ) where

import Modelling.StateDiagram.Datatype (
  Connection(..),
  UMLStateDiagram,
  unUML,
  globalise,
  )

import Modelling.StateDiagram.Checkers.Helpers (inCompoundState, notHistory)

checkHistory :: UMLStateDiagram n Int -> Maybe String
checkHistory a
  | not (checkEdge False a) =
      Just "History should never be reached from (somewhere, possibly nested) inside their own compound state"
  | not (checkEdge True a) =
      Just "History should only have connections towards something in their own compound state or deeper"
  | otherwise =
      Nothing

checkEdge :: Bool -> UMLStateDiagram n Int -> Bool
checkEdge out =
  unUML (\_ sub conn _ ->
           if out then
             all (\Connection{pointFrom, pointTo} ->
                    inCompoundState pointFrom pointTo)
           $ filter (not . (`notHistory` sub) . pointFrom) conn
           else
             all (\Connection{pointFrom, pointTo} ->
                    not (inCompoundState pointTo pointFrom))
           $ filter (not . (`notHistory` sub) . pointTo) conn
        )
  . globalise
