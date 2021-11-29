module GenerateSpec (spec) where

import Generate
import ExampleSpec (allTheCheckers)

import Test.QuickCheck
import Data.List.Extra

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "randomSD" $
    prop ("generates valid diagram expressions") $
      forAll randomSD $ \(code,_) -> firstJust id (map (($ code) . snd) allTheCheckers) `shouldBe` Nothing
  describe "randomSD" $
    prop ("doesn't retry too often") $
      forAll randomSD $ \(_,attempts) -> attempts `shouldSatisfy` (< 50)
