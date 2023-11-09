{-# OPTIONS_GHC -Wno-error=deprecations #-}

module Modelling.StateDiagram.Checkers.Crossings ( checkCrossings ) where

import Modelling.StateDiagram.Datatype (
  UMLStateDiagram(unUML'),
  localise,
  )

checkCrossings :: UMLStateDiagram n Int -> Maybe String
checkCrossings s = case countConnections (unUML' s) - countConnections (localise (unUML' s)) of
  0 -> Nothing
  n -> Just $ "Has " ++ show n ++ " illegal crossing(s) between regions"
  where
    countConnections = sum . fmap length
