-- |

module ExampleSpec where

import Example
import Test

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "checkConnection " $ do
    it "isSuccessful for slide246" $
      checkConnection  slide246 `shouldBe` Nothing
    it "isSuccessful for slide253 " $
      checkConnection  slide253 `shouldBe` Nothing
    it "isSuccessful for slide257" $
      checkConnection  slide257 `shouldBe` Nothing
    it "isSuccessful for slide267a" $
      checkConnection  slide267a `shouldBe` Nothing
    it "isSuccessful for slide267b" $
      checkConnection  slide267b `shouldBe` Nothing
    it "isSuccessful for slide271" $
      checkConnection  slide271 `shouldBe` Nothing
    it "isSuccessful for slide273" $
      checkConnection  slide273 `shouldBe` Nothing
    it "isSuccessful for slide275" $
      checkConnection  slide275 `shouldBe` Nothing
    it "isSuccessful for slide277" $
      checkConnection  slide277 `shouldBe` Nothing
    it "isSuccessful for slide278" $
      checkConnection  slide278 `shouldBe` Nothing
    it "isSuccessful for slide279" $
      checkConnection  slide279 `shouldBe` Nothing
    it "isSuccessful for slide280" $
      checkConnection  slide280 `shouldBe` Nothing
    it "isSuccessful for slide281" $
      checkConnection  slide281 `shouldBe` Nothing
    it "isSuccessful for slide283" $
      checkConnection  slide283 `shouldBe` Nothing
    it "isSuccessful for task26a" $
      checkConnection  task26a `shouldBe` Nothing
    it "isSuccessful for task26b" $
      checkConnection  task26b `shouldBe` Nothing
    it "isSuccessful for task27" $
      checkConnection  task27 `shouldBe` Nothing
    it "isSuccessful for task85" $
      checkConnection  task85 `shouldBe` Nothing
    it "isSuccessful for task88" $
      checkConnection  task88 `shouldBe` Nothing
    it "isSuccessful for test4" $
      checkConnection  test4 `shouldBe` Nothing
    it "isSuccessful for picture1" $
         checkConnection   picture1 `shouldBe` Nothing
    it "isSuccessful for picture2" $
         checkConnection   picture2 `shouldBe` Nothing
    it "isSuccessful for picture3" $
         checkConnection   picture3 `shouldBe` Nothing
    it "isSuccessful for picture4" $
         checkConnection   picture4 `shouldBe` Nothing
  describe "checkUniqueness" $ do
    it "isSuccessful for slide246" $
      checkUniqueness slide246 `shouldBe` Nothing
    it "isSuccessful for slide253 " $
      checkUniqueness slide253 `shouldBe` Nothing
    it "isSuccessful for slide257" $
      checkUniqueness slide257 `shouldBe` Nothing
    it "isSuccessful for slide267a" $
      checkUniqueness slide267a `shouldBe` Nothing
    it "isSuccessful for slide267b" $
      checkUniqueness slide267b `shouldBe` Nothing
    it "isSuccessful for slide271" $
      checkUniqueness slide271 `shouldBe` Nothing
    it "isSuccessful for slide273" $
      checkUniqueness slide273 `shouldBe` Nothing
    it "isSuccessful for slide275" $
      checkUniqueness slide275 `shouldBe` Nothing
    it "isSuccessful for slide277" $
      checkUniqueness slide277 `shouldBe` Nothing
    it "isSuccessful for slide278" $
      checkUniqueness slide278 `shouldBe` Nothing
    it "isSuccessful for slide279" $
      checkUniqueness slide279 `shouldBe` Nothing
    it "isSuccessful for slide280" $
      checkUniqueness slide280 `shouldBe` Nothing
    it "isSuccessful for slide281" $
      checkUniqueness slide281 `shouldBe` Nothing
    it "isSuccessful for slide283" $
      checkUniqueness slide283 `shouldBe` Nothing
    it "isSuccessful for task26a" $
      checkUniqueness task26a `shouldBe` Nothing
    it "isSuccessful for task26b" $
      checkUniqueness task26b `shouldBe` Nothing
    it "isSuccessful for task27" $
      checkUniqueness task27 `shouldBe` Nothing
    it "isSuccessful for task85" $
      checkUniqueness task85 `shouldBe` Nothing
    it "isSuccessful for task88" $
      checkUniqueness task88 `shouldBe` Nothing
    it "isSuccessful for test4" $
      checkUniqueness test4 `shouldBe` Nothing
    it "isSuccessful for picture1" $
         checkUniqueness  picture1 `shouldBe` Nothing
    it "isSuccessful for picture2" $
         checkUniqueness  picture2 `shouldBe` Nothing
    it "isSuccessful for picture3" $
         checkUniqueness  picture3 `shouldBe` Nothing
    it "isSuccessful for picture4" $
         checkUniqueness  picture4 `shouldBe` Nothing
  describe "checkValidity" $ do
    it "isSuccessful for slide246" $
      checkValidity slide246 `shouldBe` Nothing
    it "isSuccessful for slide253 " $
      checkValidity slide253 `shouldBe` Nothing
    it "isSuccessful for slide257" $
      checkValidity slide257 `shouldBe` Nothing
    it "isSuccessful for slide267a" $
      checkValidity slide267a `shouldBe` Nothing
    it "isSuccessful for slide267b" $
      checkValidity slide267b `shouldBe` Nothing
    it "isSuccessful for slide271" $
      checkValidity slide271 `shouldBe` Nothing
    it "isSuccessful for slide273" $
      checkValidity slide273 `shouldBe` Nothing
    it "isSuccessful for slide275" $
      checkValidity slide275 `shouldBe` Nothing
    it "isSuccessful for slide277" $
      checkValidity slide277 `shouldBe` Nothing
    it "isSuccessful for slide278" $
      checkValidity slide278 `shouldBe` Nothing
    it "isSuccessful for slide279" $
      checkValidity slide279 `shouldBe` Nothing
    it "isSuccessful for slide280" $
      checkValidity slide280 `shouldBe` Nothing
    it "isSuccessful for slide281" $
      checkValidity slide281 `shouldBe` Nothing
    it "isSuccessful for slide283" $
      checkValidity slide283 `shouldBe` Nothing
    it "isSuccessful for task26a" $
      checkValidity task26a `shouldBe` Nothing
    it "isSuccessful for task26b" $
      checkValidity task26b `shouldBe` Nothing
    it "isSuccessful for task27" $
      checkValidity task27 `shouldBe` Nothing
    it "isSuccessful for task85" $
      checkValidity task85 `shouldBe` Nothing
    it "isSuccessful for task88" $
      checkValidity task88 `shouldBe` Nothing
    it "isSuccessful for test4" $
      checkValidity test4 `shouldBe` Nothing
  describe "checkWrapper" $ do
    it "isSuccessful for slide246" $
      checkWrapper slide246 `shouldBe` Nothing
    it "isSuccessful for slide253 " $
      checkWrapper slide253 `shouldBe` Nothing
    it "isSuccessful for slide257" $
      checkWrapper slide257 `shouldBe` Nothing
    it "isSuccessful for slide267a" $
      checkWrapper slide267a `shouldBe` Nothing
    it "isSuccessful for slide267b" $
      checkWrapper slide267b `shouldBe` Nothing
    it "isSuccessful for slide271" $
      checkWrapper slide271 `shouldBe` Nothing
    it "isSuccessful for slide273" $
      checkWrapper slide273 `shouldBe` Nothing
    it "isSuccessful for slide275" $
      checkWrapper slide275 `shouldBe` Nothing
    it "isSuccessful for slide277" $
      checkWrapper slide277 `shouldBe` Nothing
    it "isSuccessful for slide278" $
      checkWrapper slide278 `shouldBe` Nothing
    it "isSuccessful for slide279" $
      checkWrapper slide279 `shouldBe` Nothing
    it "isSuccessful for slide280" $
      checkWrapper slide280 `shouldBe` Nothing
    it "isSuccessful for slide281" $
      checkWrapper slide281 `shouldBe` Nothing
    it "isSuccessful for slide283" $
      checkWrapper slide283 `shouldBe` Nothing
    it "isSuccessful for task26a" $
      checkWrapper task26a `shouldBe` Nothing
    it "isSuccessful for task26b" $
      checkWrapper task26b `shouldBe` Nothing
    it "isSuccessful for task27" $
      checkWrapper task27 `shouldBe` Nothing
    it "isSuccessful for task85" $
      checkWrapper task85 `shouldBe` Nothing
    it "isSuccessful for task88" $
      checkWrapper task88 `shouldBe` Nothing
    it "isSuccessful for test1" $
      checkWrapper test1 `shouldBe` Nothing

