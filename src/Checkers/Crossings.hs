module Checkers.Crossings ( checkCrossings ) where

import Datatype (
  UMLStateDiagram,
  localise,
  )

checkCrossings :: UMLStateDiagram -> Maybe String
checkCrossings s = case connections s - connections (localise s) of
  0 -> Nothing
  n -> Just $ "Has " ++ show n ++ " illegal crossing(s) between regions"
  where
    connections = sum . fmap length
