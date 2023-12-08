{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.StateDiagram.Datatype.Lenses where

import Modelling.StateDiagram.Datatype (Connection(..))
import Control.Lens (makeLensesFor, Traversal')

$(makeLensesFor [("pointFrom", "pointFromL"), ("pointTo", "pointToL")] ''Connection)

pointBothL :: Traversal' (Connection label) [label]
pointBothL f Connection{pointFrom, pointTo, transition} =
    Connection <$> f pointFrom <*> f pointTo <*> pure transition
