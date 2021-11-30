{-# LANGUAGE NamedFieldPuns #-}

module Checkers.Structure ( checkStructure ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram,
  globalise,
  )

import Checkers.Helpers (globalStart, notHistory, isSDCD, getAllElem)

checkStructure :: UMLStateDiagram -> Maybe String
checkStructure a
  | not (checkSubstateSD a) =
      Just "Error: Substate of StateDiagram constructor cannot be empty or just History/Joint"
  | not (checkHistOutTransition a) =
      Just "Error: Outgoing edges from a history node always have the empty transition"
  | not (checkReachablity a) =
      Just "Should Not contain unreachable states (except Start states)"
  | otherwise =
      Nothing

checkSubstateSD :: UMLStateDiagram -> Bool
checkSubstateSD (CombineDiagram a _) = all checkSubstateSD a
checkSubstateSD (StateDiagram a _ _ _ _) = any checkListInSD a && all checkSubstateSD a
checkSubstateSD _ = True

checkListInSD :: UMLStateDiagram -> Bool
checkListInSD Joint {} = False
checkListInSD History {} = False
checkListInSD _ = True

checkHistOutTransition :: UMLStateDiagram -> Bool
checkHistOutTransition StateDiagram { substate, connection } = all (`checkHistConnTransition` substate) connection
                                                               && all checkHistOutTransition substate
checkHistOutTransition CombineDiagram {substate} = all checkHistOutTransition substate
checkHistOutTransition  _ = True

checkHistConnTransition :: Connection -> [UMLStateDiagram] -> Bool
checkHistConnTransition Connection { pointFrom,transition } a = null transition || notHistory pointFrom a

checkReachablity :: UMLStateDiagram -> Bool
checkReachablity s@StateDiagram{}
  = all (`elem` (map pointTo conn ++ globalStart s)) stateNoCDSD
    where
      global = globalise s
      conn   = connection global
      sub    = substate global
      stateNoCDSD = filter (not.(`isSDCD` sub)) (getAllElem s)
checkReachablity _ = True
