module ExampleSpec (spec, allTheCheckers) where

import Example (positiveExamples)
import Checkers
import Datatype (globalise, localise)
import Layout (checkWrapper)

import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad (void, forM_)
import Data.Tuple.Extra ((***))

spec :: Spec
spec =
  forM_ allTheCheckers $ \(checkerName, checkerCode) ->
    describe checkerName $ void $ sequence
      [ it ("isSuccessful for " ++ name) $ checkerCode code `shouldBe` Nothing
      | (name, code) <-
          positiveExamples
          ++ map (("'localise' of " ++) *** localise) positiveExamples
          ++ map (("'globalise' of " ++) *** globalise) positiveExamples
      ]

allTheCheckers =
  [ ("checkRepresentation", checkRepresentation)
  , ("checkStructure", checkStructure)
  , ("checkCrossings", checkCrossings)
  , ("checkNameUniqueness", checkNameUniqueness)
  , ("checkUniqueness", checkUniqueness)
  , ("checkEndState", checkEndState)
  , ("checkJoint", checkJoint)
  , ("checkHistory", checkHistory)
  , ("checkSemantics", checkSemantics)
  , ("checkWrapper", checkWrapper)
  , ("checkDrawability", checkDrawability)
  ]
