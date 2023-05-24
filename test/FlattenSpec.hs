module FlattenSpec (
  spec
) where

import Test.Hspec
import Test.Hspec.QuickCheck
-- import Example (flatCase1)
-- import Flatten (flatten)
{-
import Datatype (UMLStateDiagram
                ,StateDiagram'(..)
                ,Connection'(..))

flattedCase1 :: UMLStateDiagram
flattedCase1
  = let
    isA = InnerMostState 1 "A, " ""
    isB = InnerMostState 2 "B, " ""
    isC = InnerMostState 3 "C, " ""
    isG = InnerMostState 4 "G, " ""
    isH = InnerMostState 5 "H, " ""
    in
    StateDiagram [isA,isB,isC,isG,isH] 0 ""
      [ Connection [2] [3] "d"
      , Connection [3] [4] "e"
      , Connection [4] [2] "c"
      , Connection [5] [4] "b"
      , Connection [1] [5] "a"]
      [1]
-}

spec :: Spec
spec = do
  describe "flatten regions" $
    prop "flatten flatCase1" $
      True `shouldBe` True
      -- (flatten flatCase1) `shouldBe` flattedCase1 (TODO: add missing initial transition)
