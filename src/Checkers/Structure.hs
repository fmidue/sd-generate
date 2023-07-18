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

checkStructure :: UMLStateDiagram n Int -> Maybe String
checkStructure a
  | not (checkSubstatesSD $ unUML' a) =
      Just "Error: Substates of StateDiagram constructor cannot be empty or just History/ForkOrJoin"
  | not (checkHistOutTransition $ unUML' a) =
      Just "Error: Outgoing edges from a history node always have the empty transition"
  | not (checkReachability a) =
      Just "Should Not contain unreachable states (except Start states)"
  | otherwise =
      Nothing

checkSubstatesSD :: StateDiagram n Int [Connection Int] -> Bool
checkSubstatesSD (CombineDiagram a _) = all checkSubstatesSD a
checkSubstatesSD (StateDiagram a _ _ _ _) = any checkListInSD a && all checkSubstatesSD a
checkSubstatesSD _ = True

checkListInSD :: StateDiagram n Int [Connection Int] -> Bool
checkListInSD ForkOrJoin {} = False
checkListInSD History {} = False
checkListInSD _ = True

checkHistOutTransition :: StateDiagram n Int [Connection Int] -> Bool
checkHistOutTransition StateDiagram { substates, connections } = all (`checkHistConnTransition` substates) connections
                                                               && all checkHistOutTransition substates
checkHistOutTransition CombineDiagram { substates } = all checkHistOutTransition substates
checkHistOutTransition  _ = True

checkHistConnTransition :: Connection Int -> [StateDiagram n Int [Connection Int]] -> Bool
checkHistConnTransition Connection { pointFrom,transition } a = null transition || notHistory pointFrom a

checkReachability :: UMLStateDiagram n Int -> Bool
checkReachability s
  = all (`elem` (map pointTo conn ++ globalStart (unUML' s))) stateNoCDSD
    where
      global = unUML' $ globalise s
      conn   = connections global
      sub    = substates global
      stateNoCDSD = filter (not . (`isSDCD` sub)) (getAllElem $ unUML' s)
