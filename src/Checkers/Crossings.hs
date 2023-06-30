{-# OPTIONS_GHC -Wno-error=deprecations #-}

module Checkers.Crossings ( checkCrossings ) where

import Datatype (
  UMLStateDiagram(unUML'),
  localise,
  )

checkCrossings :: UMLStateDiagram n Int -> Maybe String
checkCrossings s = case connections (unUML' s) - connections (localise (unUML' s)) of
  0 -> Nothing
  n -> Just $ "Has " ++ show n ++ " illegal crossing(s) between regions"
  where
    connections = sum . fmap length
