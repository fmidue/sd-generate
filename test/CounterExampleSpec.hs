module CounterExampleSpec (spec) where

import CounterExample
import Test
import ExampleSpec (allTheCheckers)

import Test.Hspec (Spec, describe, it, shouldBe,shouldSatisfy)
import Control.Monad (void, forM_)
import Data.Maybe(isJust)
import Data.List (partition)

spec :: Spec
spec = do
  counterExamplesOnlyFor "checkConnection"
        [ ("outerStateDiagC1", outerStateDiagC1)
         ,("outerStateDiagC2 ", outerStateDiagC2)
         ,("outerStateDiagC3", outerStateDiagC3)
         ,("insideStateDiagC1", insideStateDiagC1)
         ,("insideStateDiagC2", insideStateDiagC2)
         ,("insideStateDiagC3", insideStateDiagC3)
         ,("insideCombineDiagC1", insideCombineDiagC1)
         ,("tooDeep", tooDeep)
         ,("smallTestC1",smallTestC1)
         ,("smallTestC",smallTestC)
        ]
  counterExamplesOnlyFor "checkNameUniqueness"
        [("forCheckNameUniqueness1", forCheckNameUniqueness1)
        ,("forCheckNameUniqueness2", forCheckNameUniqueness2)
        ]
  counterExamplesOnlyFor "checkUniqueness"
        [("outerStateDiagL1", outerStateDiagL1)
        ,("outerStateDiagL2", outerStateDiagL2)
        ,("outerStateDiagL3", outerStateDiagL3)
        ,("insideStateDiagL1", insideStateDiagL1)
        ,("insideStateDiagL2", insideStateDiagL2)
        ,("insideStateDiagL3", insideStateDiagL3)
        ,("insideCombineDiagL1", insideCombineDiagL1)
        ]
  counterExamplesOnlyFor "checkStartState"
        [ ("nonExist", nonExist)
          ,("outerStateDiag1 ", outerStateDiag1)
          ,("outerStateDiag2", outerStateDiag2)
          ,("innerStateDiag1", innerStateDiag1)
          ,("innerStateDiag2", innerStateDiag2)
          ,("innerCombineDiag1", innerCombineDiag1)
        ]
  counterExamplesOnlyFor "checkEndState"
       [ ("forCheckEndState1", forCheckEndState1 )
        ,("forCheckEndState2", forCheckEndState2)
       ]
  counterExamplesOnlyFor "checkJoint"
       [ ("forCheckJoint1", forCheckJoint1)
        ,("forCheckJoint2", forCheckJoint2)
       ]
  counterExamplesOnlyFor "checkStructure"
       [ ("outerMostCombineDiag", outerMostCombineDiag)
         ,("substateOnlyJH1", substateOnlyJH1)
         ,("substateOnlyJH2", substateOnlyJH2)
         ,("oneSD1", oneSD1)
         ,("oneSD2", oneSD2)
         ,("forCheckHistOutTransition1", forCheckHistOutTransition1)
         ,("forCheckHistOutTransition2", forCheckHistOutTransition2)
       ]
  counterExamplesOnlyFor "checkSemantics"
       [ ("forCheckSameConnection1", forCheckSameConnection1)
       ,("forCheckSameConnection2", forCheckSameConnection2)
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
      | (name, code) <- theExamples ]

allTheCheckersExceptForWrapper =
  filter (("checkWrapper" /=) . fst) allTheCheckers
