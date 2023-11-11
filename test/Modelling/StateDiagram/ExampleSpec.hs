{-# OPTIONS_GHC -Wno-error=deprecations #-}

module Modelling.StateDiagram.ExampleSpec (spec, allTheCheckers) where

import Modelling.StateDiagram.Example (positiveExamples)
import Modelling.StateDiagram.Checkers
import Modelling.StateDiagram.Datatype (UMLStateDiagram(unUML'), globalise)
import Modelling.StateDiagram.Layout (checkWrapper)

import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad (forM_)
import Data.Tuple.Extra ((***))

spec :: Spec
spec =
  forM_ allTheCheckers $ \(checkerName, checkerCode) ->
    describe checkerName $ sequence_
      [ it ("isSuccessful for " ++ name) $ checkerCode code `shouldBe` Nothing
      | (name, code) <-
          positiveExamples
          ++ map (("'globalise' of " ++) *** globalise) positiveExamples
      ]

allTheCheckers :: [(String, UMLStateDiagram String Int -> Maybe String)]
allTheCheckers =
  [ ("checkRepresentation", checkRepresentation)
  , ("checkStructure", checkStructure)
  , ("checkCrossings", checkCrossings)
  , ("checkNameUniqueness", checkNameUniqueness)
  , ("checkUniqueness", checkUniqueness)
  , ("checkEndState", checkEndState)
  , ("checkForkAndJoin", checkForkAndJoin)
  , ("checkHistory", checkHistory)
  , ("checkSemantics", checkSemantics)
  , ("checkWrapper", checkWrapper . unUML')
  , ("checkDrawability", checkDrawability)
  ]
