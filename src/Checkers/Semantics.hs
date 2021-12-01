module Checkers.Semantics ( checkSemantics ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram,
  globalise,
  )

import Checkers.Helpers (checkEmptyOutTran, checkSameOutTran, notJoint)

checkSemantics :: UMLStateDiagram -> Maybe String
checkSemantics a
  | not (checkSameConnection a) =
      Just "Error: No two connections are allowed leaving the same source and and having the same label (except From Joint Node)."
  | not (checkEmptyTran a) =
      Just "Error: The non-Joint state which has more than one outgoing connection, must have a non-empty transition label."
  | otherwise =
      Nothing

checkSameConnection :: UMLStateDiagram -> Bool
checkSameConnection s@StateDiagram {} =
                        all (`checkSameOutTran` withoutJoint) withoutJoint
                        where
                          global = globalise s
                          sub = substate global
                          conn = connection global
                          withoutJoint = filter ((`notJoint ` sub).pointFrom) conn
checkSameConnection _ = True

checkEmptyTran :: UMLStateDiagram -> Bool
checkEmptyTran s@StateDiagram {} =
                         all (`checkEmptyOutTran` withoutJoint) withoutJoint
                        where
                             global = globalise s
                             sub = substate global
                             conn = connection global
                             withoutJoint = filter ((`notJoint ` sub).pointFrom) conn
checkEmptyTran _ = True
