{-# LANGUAGE NamedFieldPuns #-}

module Checkers.Semantics ( checkSemantics ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram,
  globalise,
  )

import Checkers.Helpers

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

checkSameOutTran :: Connection -> [Connection] -> Bool
checkSameOutTran a b = length tranSame == 1
                where
                  fromSame  = filter ((pointFrom a ==).pointFrom) b
                  tranSame = filter ((transition a ==).transition) fromSame

checkEmptyTran :: UMLStateDiagram -> Bool
checkEmptyTran s@StateDiagram {} =
                         (not . any (null . transition)) filterOneOut
                        where
                             global = globalise s
                             sub = substate global
                             conn = connection global
                             withoutJoint = filter ((`notJoint ` sub).pointFrom) conn
                             filterOneOut = filter (not.(`onlyOneOut` withoutJoint)) withoutJoint
checkEmptyTran _ = True

onlyOneOut :: Connection -> [Connection] -> Bool
onlyOneOut a b = length fromSame == 1
            where
             fromSame  = filter ((pointFrom a ==).pointFrom) b
