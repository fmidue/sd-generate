{-# OPTIONS_GHC -Wno-error=deprecations #-}
{-# LANGUAGE NamedFieldPuns #-}

module Checkers.NameUniqueness ( checkNameUniqueness ) where

import Datatype (
  StateDiagram(..),
  UMLStateDiagram(unUML'),
  Connection,
  )

import Data.List.Extra

checkNameUniqueness :: UMLStateDiagram Int -> Maybe String
checkNameUniqueness a
  | not (checkSubNameUniq $ unUML' a) =
      Just "Error: At the same layer name Uniqueness is not fulfilled"
  | not (checkSDNameUniq $ unUML' a) =
      Just "Error: In each StateDiagram, the name (if not empty) should be different from all names found arbitrarily deep inside the substates."
  | otherwise =
      Nothing

checkSDNameUniq :: StateDiagram Int [Connection Int] -> Bool
checkSDNameUniq StateDiagram {substate,name} = name `notElem` getLayerName substate
                                              && all (checkDeeperUniq name) substate
                                              && all checkSDNameUniq substate
checkSDNameUniq CombineDiagram {substate} = all checkSDNameUniq substate
checkSDNameUniq  _ = True

checkDeeperUniq :: String -> StateDiagram Int [Connection Int] -> Bool
checkDeeperUniq a StateDiagram {substate} = a `notElem` getLayerName substate
                                              && all (checkDeeperUniq a) substate
checkDeeperUniq a CombineDiagram {substate} = all (checkDeeperUniq a) substate
checkDeeperUniq _ _ = True

checkSubNameUniq :: StateDiagram Int [Connection Int] -> Bool
checkSubNameUniq StateDiagram {substate} =  not (anySame (getLayerName substate))
                                            && all checkSubNameUniq  substate
checkSubNameUniq  CombineDiagram {substate} = not (anySame (getLayerName substate))
                                              && all checkSubNameUniq  substate
checkSubNameUniq  _ = True

getLayerName :: [StateDiagram Int [Connection Int]] -> [String]
getLayerName a = filter (not . null) (concatMap getName a)

getName :: StateDiagram Int [Connection Int] -> [String]
getName StateDiagram{name} = [name]
getName InnerMostState{name} = [name]
getName CombineDiagram{substate} = concatMap getName substate
getName _ = []
