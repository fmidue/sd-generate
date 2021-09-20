module GenerateSpec where

import Generate
import Test

import Test.QuickCheck
import Data.List.Extra

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "randomSD" $
    prop ("generates valid diagram expressions") $
      forAll randomSD $ \code -> firstJust id (map ($ code) [checkStructure,checkSemantics,checkUniqueness, checkStartState, checkConnection, checkWrapper]) `shouldBe` Nothing
