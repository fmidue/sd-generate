module GenerateSpec (spec) where

import Generate
import Test
import Layout (checkWrapper)

import Test.QuickCheck
import Data.List.Extra

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "randomSD" $
    prop ("generates valid diagram expressions") $
      forAll randomSD $ \code -> firstJust id (map ($ code) [checkStructure, checkSemantics, checkUniqueness, checkStartState, checkEndState, checkConnection, checkWrapper]) `shouldBe` Nothing
