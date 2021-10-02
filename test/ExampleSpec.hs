module ExampleSpec (spec, positiveExamples) where

import Example
import Test
import Layout (checkWrapper)

import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad (void)

spec :: Spec
spec = do
  describe "checkConnection" $ void $ sequence
    [ it ("isSuccessful for " ++ name) $ checkConnection code `shouldBe` Nothing
    | (name, code) <- positiveExamples ]
  describe "checkUniqueness" $ void $ sequence
    [ it ("isSuccessful for " ++ name) $ checkUniqueness code `shouldBe` Nothing
    | (name, code) <- positiveExamples ]
  describe "checkStructure" $ void $ sequence
    [ it ("isSuccessful for " ++ name) $ checkStructure code `shouldBe` Nothing
    | (name, code) <- positiveExamples ]
  describe "checkWrapper" $ void $ sequence
    [ it ("isSuccessful for " ++ name) $ checkWrapper code `shouldBe` Nothing
    | (name, code) <- positiveExamples ]
  describe "checkStartState" $ void $ sequence
    [ it ("isSuccessful for " ++ name) $ checkStartState code `shouldBe` Nothing
    | (name, code) <- positiveExamples ]
  describe "checkJoint" $ void $ sequence
    [ it ("isSuccessful for " ++ name) $ checkJoint code `shouldBe` Nothing
    | (name, code) <- positiveExamples ]
  describe "checkEndState" $ void $ sequence
    [ it ("isSuccessful for " ++ name) $ checkEndState code `shouldBe` Nothing
    | (name, code) <- positiveExamples ]
  describe "checkSemantics" $ void $ sequence
    [ it ("isSuccessful for " ++ name) $ checkSemantics code `shouldBe` Nothing
    | (name, code) <- positiveExamples ]
  describe "checkNameUniqueness" $ void $ sequence
    [ it ("isSuccessful for " ++ name) $ checkNameUniqueness code `shouldBe` Nothing
    | (name, code) <- positiveExamples ]

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
        , ("test1", test1)
        , ("test2", test2)
        , ("test3", test3)
        , ("test4", test4)
        , ("picture1", picture1)
        , ("picture2", picture2)
        , ("picture3", picture3)
        , ("picture4", picture4)
        , ("forCheckSameConnection", forCheckSameConnection)
        ]
