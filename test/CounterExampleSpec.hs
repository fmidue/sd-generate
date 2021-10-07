module CounterExampleSpec (spec) where

import CounterExample
import Test
import ExampleSpec (allTheCheckers)
import Datatype (globalise, localise, StateDiagram(substate))
import DatatypeSpec (connectionsEmpty)

import Test.Hspec (Spec, describe, it, shouldBe,shouldSatisfy)
import Control.Monad (void, forM_)
import Data.Maybe (isJust, isNothing)
import Data.List (partition)
import Data.Tuple.Extra ((***))

spec :: Spec
spec = do
  counterExamplesOnlyFor "checkCrossings"
    [ ("bogusExample", bogusExample)
    ]
  counterExamplesOnlyFor "checkConnection"
        [ ("forCheckConnection1", forCheckConnection1)
         ,("forCheckConnection2", forCheckConnection2)
         ,("forCheckConnection3", forCheckConnection3)
         ,("forCheckConnection4", forCheckConnection4)
         ,("forCheckConnection5", forCheckConnection5)
         ,("forCheckConnection6", forCheckConnection6)
         ,("forCheckConnection7", forCheckConnection7)
         ,("forCheckConnection8", forCheckConnection8)
         ,("forCheckConnFromToRegion1", forCheckConnFromToRegion1)
         ,("forCheckConnFromToRegion2", forCheckConnFromToRegion2)
        ]
  counterExamplesOnlyFor "checkNameUniqueness"
        [("forCheckNameUniqueness1", forCheckNameUniqueness1)
        ,("forCheckNameUniqueness2", forCheckNameUniqueness2)
        ]
  counterExamplesOnlyFor "checkUniqueness"
        [("forCheckUniqueness1", forCheckUniqueness1)
        ,("forCheckUniqueness2", forCheckUniqueness2)
        ,("forCheckUniqueness3", forCheckUniqueness3)
        ,("forCheckUniqueness4", forCheckUniqueness4)
        ,("forCheckUniqueness5", forCheckUniqueness5)
        ,("forCheckUniqueness6", forCheckUniqueness6)
        ,("forCheckUniqueness7", forCheckUniqueness7)
        ]
  counterExamplesOnlyFor "checkStartState"
        [ ("forCheckSubS1", forCheckSubS1)
          ,("forCheckSubS2",forCheckSubS2)
          ,("forCheckSubS3",forCheckSubS3)
          ,("forCheckSubS4",forCheckSubS4)
          ,("forCheckSubS5",forCheckSubS5)
          ,("forCheckSubS6",forCheckSubS6)
        ]
  counterExamplesOnlyFor "checkEndState"
       [ ("forCheckEndState1", forCheckEndState1 )
        ,("forCheckEndState2", forCheckEndState2)
       ]
  counterExamplesOnlyFor "checkJoint"
       [ ("forCheckInEdge1", forCheckInEdge1)
        ,("forCheckOutEdge1", forCheckOutEdge1)
        ,("forCheckJoint3", forCheckJoint3)
        ,("forCheckJoint4", forCheckJoint4)
       ]
  counterExamplesOnlyFor "checkStructure"
       [ ("forCheckOuterMostLayer", forCheckOuterMostLayer)
         ,("forCheckSubstateSD1", forCheckSubstateSD1)
         ,("forCheckSubstateSD2", forCheckSubstateSD2)
         ,("forCheckSubstateCD1", forCheckSubstateCD1)
         ,("forCheckSubstateCD2", forCheckSubstateCD2)
         ,("forCheckHistOutTransition1", forCheckHistOutTransition1)
         ,("forCheckHistOutTransition2", forCheckHistOutTransition2)
         -- ,("forCheckEmptyConnPoint1", forCheckEmptyConnPoint1)
         -- ,("forCheckEmptyConnPoint2", forCheckEmptyConnPoint2)
       ]
  counterExamplesOnlyFor "checkSemantics"
       [ ("forCheckSameConnection1", forCheckSameConnection1)
       ,("forCheckSameConnection2", forCheckSameConnection2)
       ,("forCheckEmptyTran1", forCheckEmptyTran1)
       ,("forCheckEmptyTran2", forCheckEmptyTran2)
       ]

counterExamplesOnlyFor theChecker theExamples = do
  let
    (negative, positives) = partition ((theChecker ==) . fst) allTheCheckersExceptForWrapper
  forM_ negative $ \(checkerName, checkerCode) ->
    describe checkerName $ void $ sequence
      [ it ("rejects " ++ name) $ checkerCode code `shouldSatisfy` isJust
      | (name, code) <- theExamples
      ]
  forM_ positives $ \(checkerName, checkerCode) ->
    describe checkerName $ void $ sequence
      [ it ("isSuccessful for " ++ name) $ checkerCode code `shouldBe` Nothing
      | (name, code) <-
          theExamples `passing` [checkStructure]
          ++ map (("'localise' of " ++) *** localise) (theExamples `passing` [checkUniqueness, checkConnection])
          ++ map (("'globalise' of " ++) *** globalise) (theExamples `passing` [checkUniqueness])
      ]
  describe "localise/globalise" $ void $ sequence
    [ it ("are each others' inverses in a sense, on " ++ name) $ localise (globalise code) `shouldBe` localise code
    | (name, code) <- theExamples `passing` [checkUniqueness, checkConnection] ]
  describe "globalise/localise" $ void $ sequence
    [ it ("are each others' inverses in a sense, on " ++ name) $ globalise (localise code) `shouldBe` globalise code
    | (name, code) <- theExamples `passing` [checkUniqueness, checkConnection, checkCrossings] ]
  describe "globalise" $ void $ sequence
    [ it ("doesn't leave any connections inside, on " ++ name) $ all connectionsEmpty (substate (globalise code))
    | (name, code) <- theExamples `passing` [checkStructure] ]
  where
    passing = flip $ \checkers -> filter (all isNothing . (`map` checkers) . flip ($) . snd)

allTheCheckersExceptForWrapper =
  filter (("checkWrapper" /=) . fst) allTheCheckers
