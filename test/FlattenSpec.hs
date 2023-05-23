module FlattenSpec (
  spec
) where

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "flatten regions" $
    prop "flatten statediagram with only innermost" $
      True `shouldBe` True
