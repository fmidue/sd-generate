module DatatypeSpec (spec) where

import Datatype
import ExampleSpec (positiveExamples)

import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad (void)

spec :: Spec
spec = do
  describe "localise/globalise" $ void $ sequence
    [ it ("are each others' inverses in a sense, on " ++ name) $ localise (globalise code) `shouldBe` code
    | (name, code) <- positiveExamples ]
