module Modelling.StateDiagram.ExampleSpec (spec) where

import Modelling.StateDiagram.Example (positiveExamples)
import Modelling.StateDiagram.Checkers (allTheCheckers)
import Modelling.StateDiagram.Datatype (globalise)

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
