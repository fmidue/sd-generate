{-# OPTIONS_GHC -Wno-error=incomplete-patterns #-}

module Checkers.Semantics ( checkSemantics ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram(unUML),
  globalise,
  )

import Checkers.Helpers (checkEmptyOutTran, checkSameOutTran, notJoint)

checkSemantics :: UMLStateDiagram Int -> Maybe String
checkSemantics a
  | not (checkSameConnection a) =
      Just "Error: No two connections are allowed leaving the same source and and having the same label (except From Joint Node)."
  | not (checkEmptyTran a) =
      Just "Error: The non-Joint state which has more than one outgoing connection, must have a non-empty transition label."
  | otherwise =
      Nothing

checkSameConnection :: UMLStateDiagram Int -> Bool
checkSameConnection s =
                        all (`checkSameOutTran` withoutJoint) withoutJoint
                        where
                          global = unUML $ globalise s
                          sub = substate global
                          conn = connection global
                          withoutJoint = filter ((`notJoint ` sub).pointFrom) conn

checkEmptyTran :: UMLStateDiagram Int -> Bool
checkEmptyTran s =
                         all (`checkEmptyOutTran` withoutJoint) withoutJoint
                        where
                             global = unUML $ globalise s
                             sub = substate global
                             conn = connection global
                             withoutJoint = filter ((`notJoint ` sub).pointFrom) conn
