module CounterExampleSpec where

import CounterExample
import Test

import Test.Hspec (Spec, describe, it, shouldBe,shouldSatisfy)
import Control.Monad (void)
import Data.Maybe(isJust)

spec :: Spec
spec = do
  describe "checkConnection" $ void $ sequence
    [ it ("rejects " ++ name) $ checkConnection code `shouldSatisfy` isJust
    | (name, code) <-
        [ ("outerStateDiagC1", outerStateDiagC1)
         ,("outerStateDiagC2 ", outerStateDiagC2)
         ,("outerStateDiagC3", outerStateDiagC3)
         ,("insideStateDiagC1", insideStateDiagC1)
         ,("insideStateDiagC2", insideStateDiagC2)
         ,("insideStateDiagC3", insideStateDiagC3)
         ,("outerCombineDiagC1", outerCombineDiagC1)
         ,("insideCombineDiagC1", insideCombineDiagC1)
         ,("tooDeep", tooDeep)
        ]
    ]
  describe "checkUniqueness" $ void $ sequence
    [ it ("rejects " ++ name) $ checkUniqueness code `shouldSatisfy` isJust
    | (name, code) <-
        [("outerStateDiagL1", outerStateDiagL1)
        ,("outerStateDiagL2", outerStateDiagL2)
        ,("outerStateDiagL3", outerStateDiagL3)
        ,("insideStateDiagL1", insideStateDiagL1)
        ,("insideStateDiagL2", insideStateDiagL2)
        ,("insideStateDiagL3", insideStateDiagL3)
        ,("outerCombineDiagL1", outerCombineDiagL1)
        ,("insideCombineDiagL1", insideCombineDiagL1)
        ]
    ]
  describe "checkStartState" $ void $ sequence
    [ it ("rejects " ++ name) $ checkStartState code `shouldSatisfy` isJust
    | (name, code) <-
        [ ("nonExist", nonExist)
          ,("outerStateDiag1 ", outerStateDiag1)
          ,("outerStateDiag2", outerStateDiag2)
          ,("innerStateDiag1", innerStateDiag1)
          ,("innerStateDiag2", innerStateDiag2)
          ,("innerCombineDiag1", innerCombineDiag1)
          ,("outerCombineDiag1", outerCombineDiag1)
        ]
    ]
