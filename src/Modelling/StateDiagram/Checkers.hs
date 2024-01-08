{-# OPTIONS_GHC -Wno-error=deprecations #-}

module Modelling.StateDiagram.Checkers
  ( module Modelling.StateDiagram.Checkers.Representation
  , module Modelling.StateDiagram.Checkers.Structure
  , module Modelling.StateDiagram.Checkers.Crossings
  , module Modelling.StateDiagram.Checkers.Drawability
  , module Modelling.StateDiagram.Checkers.NameUniqueness
  , module Modelling.StateDiagram.Checkers.Uniqueness
  , module Modelling.StateDiagram.Checkers.EndState
  , module Modelling.StateDiagram.Checkers.ForkOrJoin
  , module Modelling.StateDiagram.Checkers.History
  , module Modelling.StateDiagram.Checkers.Semantics
  , allTheCheckers
  ) where

import Modelling.StateDiagram.Checkers.Representation
import Modelling.StateDiagram.Checkers.Structure
import Modelling.StateDiagram.Checkers.Crossings
import Modelling.StateDiagram.Checkers.Drawability
import Modelling.StateDiagram.Checkers.NameUniqueness
import Modelling.StateDiagram.Checkers.Uniqueness
import Modelling.StateDiagram.Checkers.EndState
import Modelling.StateDiagram.Checkers.ForkOrJoin
import Modelling.StateDiagram.Checkers.History
import Modelling.StateDiagram.Checkers.Semantics

import Modelling.StateDiagram.Datatype (UMLStateDiagram(unUML'))
import Modelling.StateDiagram.Layout (checkWrapper)

allTheCheckers :: [(String, UMLStateDiagram String Int -> Maybe String)]
allTheCheckers =
  [ ("checkRepresentation", checkRepresentation)
  , ("checkStructure", checkStructure)
  , ("checkCrossings", checkCrossings)
  , ("checkDrawability", checkDrawability)
  , ("checkNameUniqueness", checkNameUniqueness)
  , ("checkUniqueness", checkUniqueness)
  , ("checkEndState", checkEndState)
  , ("checkForkAndJoin", checkForkAndJoin)
  , ("checkHistory", checkHistory)
  , ("checkSemantics", checkSemantics)
  , ("checkWrapper", checkWrapper . unUML')
  ]
