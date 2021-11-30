module CounterExampleSpec (spec) where

import CounterExample
import Checkers (checkStructure, checkCrossings, checkUniqueness, checkDrawability)
import ExampleSpec (allTheCheckers)
import Datatype (globalise, localise, StateDiagram(substate))

import Test.Hspec (Spec, describe, it, shouldBe,shouldSatisfy)
import Control.Monad (void, forM_)
import Data.Maybe (isJust, isNothing)
import Data.List (partition, sort)
import Data.Tuple.Extra ((***))

spec :: Spec
spec = do
  counterExamplesOnlyFor "checkRepresentation"
     [ ("forCheckOuterMostLayer", forCheckOuterMostLayer)
       ,("forCheckSubstateCD1", forCheckSubstateCD1)
       ,("forCheckSubstateCD2", forCheckSubstateCD2)
       -- ,("forCheckEmptyConnPoint1", forCheckEmptyConnPoint1)
       -- ,("forCheckEmptyConnPoint2", forCheckEmptyConnPoint2)
       ,("forCheckConnection1", forCheckConnection1)
       ,("forCheckConnection2", forCheckConnection2)
       ,("forCheckConnection3", forCheckConnection3)
       ,("forCheckConnection4", forCheckConnection4)
       ,("forCheckConnection5", forCheckConnection5)
       ,("forCheckConnection6", forCheckConnection6)
       ,("forCheckConnection7", forCheckConnection7)
       ,("forCheckConnection8", forCheckConnection8)
       ,("forCheckConnFromToRegion1", forCheckConnFromToRegion1)
       ,("forCheckConnFromToRegion2", forCheckConnFromToRegion2)
       ,("forCheckConnFromToRegion3", forCheckConnFromToRegion3)
       ,("forCheckConnFromToRegion4", forCheckConnFromToRegion4)
       ,("forCheckSubS1", forCheckSubS1)
       ,("forCheckSubS2",forCheckSubS2)
       ,("forCheckSubS3",forCheckSubS3)
       ,("forCheckSubS4",forCheckSubS4)
       ,("forCheckSubS5",forCheckSubS5)
       ,("forCheckSubS6",forCheckSubS6)
       ,("forCheckStartToRegion1",forCheckStartToRegion1)
       ,("forCheckStartToRegion2",forCheckStartToRegion2)
     ]
  counterExamplesOnlyFor "checkStructure"
     [ ("forCheckSubstateSD1", forCheckSubstateSD1)
       ,("forCheckSubstateSD2", forCheckSubstateSD2)
       ,("forCheckSubstateSD3", forCheckSubstateSD3)
       ,("forCheckHistOutTransition1", forCheckHistOutTransition1)
       ,("forCheckHistOutTransition2", forCheckHistOutTransition2)
       ,("forCheckHistOutTransition3", forCheckHistOutTransition3)
       ,("forCheckReachablity1", forCheckReachablity1)
       ,("forCheckReachablity2", forCheckReachablity2)
       ,("forCheckReachablity3", forCheckReachablity3)
     ]
  counterExamplesOnlyFor "checkCrossings"
    [ ("bogusExample", bogusExample)
      ,("foCheckCrossings1", foCheckCrossings1)
    ]
  counterExamplesOnlyFor "checkNameUniqueness"
        [("forCheckNameUniqueness1", forCheckNameUniqueness1)
        ,("forCheckNameUniqueness2", forCheckNameUniqueness2)
        ,("forCheckSubNameUniq2", forCheckSubNameUniq2)
        ,("forCheckSDNameUniq2", forCheckSDNameUniq2)
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
  counterExamplesOnlyFor "checkEndState"
       [ ("forCheckEndState1", forCheckEndState1)
        ,("forCheckEndState2", forCheckEndState2)
        ,("forCheckEndState3", forCheckEndState3)
       ]
  counterExamplesOnlyFor "checkJoint"
       [ ("forCheckTransition1", forCheckTransition1)
        ,("forCheckTransition2", forCheckTransition2)
        ,("forCheckMtoOne1", forCheckMtoOne1)
        ,("forCheckMtoOne2", forCheckMtoOne2)
        ,("forCheckMtoOne3", forCheckMtoOne3)
        ,("forCheckMtoOne4", forCheckMtoOne4)
        ,("forCheckMtoOne5", forCheckMtoOne5)
        ,("forCheckMtoOne6", forCheckMtoOne6)
        ,("forCheckMtoOne7", forCheckMtoOne7)
        ,("forCheckMtoOne8", forCheckMtoOne8)
        ,("forCheckTransition3", forCheckTransition3)
        ,("forCheckTransition4", forCheckTransition4)
        ,("forCheckTransition5", forCheckTransition5)
        ,("forAllgoIntoParallelRegions1", forAllgoIntoParallelRegions1)
        ,("forAllgoIntoParallelRegions2", forAllgoIntoParallelRegions2)
        ,("forAllcomeOutOfParallelRegions1", forAllcomeOutOfParallelRegions1)
       ]
  counterExamplesOnlyFor "checkHistory"
       [ ("forCheckInEdge1", forCheckInEdge1 )
         ,("forCheckInEdge2", forCheckInEdge2)
         ,("forCheckInEdge3", forCheckInEdge3)
         ,("forCheckOutEdge1", forCheckOutEdge1)
         ,("forCheckOutEdge2", forCheckOutEdge2)
       ]
  counterExamplesOnlyFor "checkSemantics"
       [ ("forCheckSameConnection1", forCheckSameConnection1)
       ,("forCheckSameConnection2", forCheckSameConnection2)
       ,("forCheckSameConnection3", forCheckSameConnection3)
       ,("forCheckEmptyTran1", forCheckEmptyTran1)
       ,("forCheckEmptyTran2", forCheckEmptyTran2)
       ,("forCheckEmptyTran3", forCheckEmptyTran3)
       ]
  describe "checkDrawability" $
    it "rejects forCheckDrawability" $ checkDrawability forCheckDrawability `shouldSatisfy` isJust
  forM_ (filter ((`notElem` ["checkDrawability", "checkStructure"]) . fst) allTheCheckers) $ \(checkerName, checkerCode) ->
    describe checkerName $
      it "isSuccessful for forCheckDrawability" $ checkerCode forCheckDrawability `shouldBe` Nothing

counterExamplesOnlyFor theChecker theExamples = do
  let
    (negative, positives) = partition ((theChecker ==) . fst) allTheCheckersExceptForBlackbox
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
          ++ map (("'localise' of " ++) *** localise) (theExamples `passing` [checkUniqueness, checkCrossings])
          ++ map (("'globalise' of " ++) *** globalise) (theExamples `passing` [checkUniqueness])
      ]
  describe "localise/globalise" $ void $ sequence
    [ it ("are each others' inverses in a sense, on " ++ name) $ fmap sort (localise (globalise code)) `shouldBe` fmap sort (localise code)
    | (name, code) <- theExamples `passing` [checkUniqueness] ]
  describe "globalise/localise" $ void $ sequence
    [ it ("are each others' inverses in a sense, on " ++ name) $ fmap sort (globalise (localise code)) `shouldBe` fmap sort (globalise code)
    | (name, code) <- theExamples `passing` [checkCrossings] ]
  where
    passing = flip $ \checkers -> filter (all isNothing . (`map` checkers) . flip ($) . snd)

allTheCheckersExceptForBlackbox =
  filter ((`notElem` ["checkWrapper", "checkDrawability"]) . fst) allTheCheckers
