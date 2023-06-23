{-# OPTIONS_GHC -Wno-error=incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-error=deprecations #-}
{-# LANGUAGE NamedFieldPuns #-}

module Checkers.Structure ( checkStructure ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram(unUML'),
  globalise,
  )

import Checkers.Helpers (globalStart, notHistory, isSDCD, getAllElem)

checkStructure :: UMLStateDiagram Int -> Maybe String
checkStructure a
  | not (checkSubstateSD $ unUML' a) =
      Just "Error: Substate of StateDiagram constructor cannot be empty or just History/Joint"
  | not (checkHistOutTransition $ unUML' a) =
      Just "Error: Outgoing edges from a history node always have the empty transition"
  | not (checkReachability a) =
      Just "Should Not contain unreachable states (except Start states)"
  | otherwise =
      Nothing

checkSubstateSD :: StateDiagram Int [Connection Int] -> Bool
checkSubstateSD (CombineDiagram a _) = all checkSubstateSD a
checkSubstateSD (StateDiagram a _ _ _ _) = any checkListInSD a && all checkSubstateSD a
checkSubstateSD _ = True

checkListInSD :: StateDiagram Int [Connection Int] -> Bool
checkListInSD Joint {} = False
checkListInSD History {} = False
checkListInSD _ = True

checkHistOutTransition :: StateDiagram Int [Connection Int] -> Bool
checkHistOutTransition StateDiagram { substate, connection } = all (`checkHistConnTransition` substate) connection
                                                               && all checkHistOutTransition substate
checkHistOutTransition CombineDiagram {substate} = all checkHistOutTransition substate
checkHistOutTransition  _ = True

checkHistConnTransition :: Connection Int -> [StateDiagram Int [Connection Int]] -> Bool
checkHistConnTransition Connection { pointFrom,transition } a = null transition || notHistory pointFrom a

checkReachability :: UMLStateDiagram Int -> Bool
checkReachability s
  = all (`elem` (map pointTo conn ++ globalStart (unUML' s))) stateNoCDSD
    where
      global = unUML' $ globalise s
      conn   = connection global
      sub    = substate global
      stateNoCDSD = filter (not . (`isSDCD` sub)) (getAllElem $ unUML' s)
