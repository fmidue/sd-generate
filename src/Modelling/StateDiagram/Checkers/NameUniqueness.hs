{-# OPTIONS_GHC -Wno-error=deprecations #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.StateDiagram.Checkers.NameUniqueness ( checkNameUniqueness ) where

import Modelling.StateDiagram.Datatype (
  StateDiagram(..),
  UMLStateDiagram(unUML'),
  Connection,
  )

import Data.List.Extra

checkNameUniqueness :: UMLStateDiagram String Int -> Maybe String
checkNameUniqueness a
  | not (checkSubNameUniq $ unUML' a) =
      Just "Error: At the same layer name Uniqueness is not fulfilled"
  | not (checkSDNameUniq $ unUML' a) =
      Just "Error: In each StateDiagram, the name (if not empty) should be different from all names found arbitrarily deep inside the substates."
  | otherwise =
      Nothing

checkSDNameUniq :: StateDiagram String Int [Connection Int] -> Bool
checkSDNameUniq StateDiagram {substates, name} = name `notElem` getLayerName substates
                                              && all (checkDeeperUniq name) substates
                                              && all checkSDNameUniq substates
checkSDNameUniq CombineDiagram {substates} = all checkSDNameUniq substates
checkSDNameUniq  _ = True

checkDeeperUniq :: String -> StateDiagram String Int [Connection Int] -> Bool
checkDeeperUniq a StateDiagram {substates} = a `notElem` getLayerName substates
                                              && all (checkDeeperUniq a) substates
checkDeeperUniq a CombineDiagram {substates} = all (checkDeeperUniq a) substates
checkDeeperUniq _ _ = True

checkSubNameUniq :: StateDiagram String Int [Connection Int] -> Bool
checkSubNameUniq StateDiagram {substates} = not (anySame (getLayerName substates))
                                            && all checkSubNameUniq substates
checkSubNameUniq  CombineDiagram {substates} = not (anySame (getLayerName substates))
                                              && all checkSubNameUniq substates
checkSubNameUniq  _ = True

getLayerName :: [StateDiagram String Int [Connection Int]] -> [String]
getLayerName a = filter (not . null) (concatMap getName a)

getName :: StateDiagram String Int [Connection Int] -> [String]
getName StateDiagram{name} = [name]
getName InnerMostState{name} = [name]
getName CombineDiagram{substates} = concatMap getName substates
getName _ = []
