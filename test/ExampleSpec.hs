module ExampleSpec (spec, positiveExamples, allTheCheckers) where

import Example
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
  ]

positiveExamples =
        [ ("verySmall", verySmall)
        , ("slide246", slide246)
        , ("slide253", slide253)
        , ("slide257", slide257)
        , ("slide267a", slide267a)
        , ("slide267b", slide267b)
        , ("slide271", slide271)
        , ("slide273", slide273)
        , ("slide275", slide275)
        , ("slide277", slide277)
        , ("slide278", slide278)
        , ("slide279", slide279)
        , ("slide280", slide280)
        , ("slide281", slide281)
        , ("slide283", slide283)
        , ("task26a", task26a)
        , ("task26b", task26b)
        , ("task27", task27)
        , ("task85", task85)
        , ("task88", task88)
        , ("test2", test2)
        , ("test4", test4)
        , ("picture1", picture1)
        , ("picture2", picture2)
        , ("picture3", picture3)
        , ("picture4", picture4)
        ]
