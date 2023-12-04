{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Modelling.StateDiagram.EnumArrowsSpec (spec)
where

import Test.Hspec (shouldBe
                  ,it
                  ,describe
                  ,Spec
                  ,pendingWith
                  ,context)
import Modelling.StateDiagram.EnumArrows(defaultEnumInstance
                                        ,enumArrowsSolution
                                        ,rate
                                        )
import Modelling.StateDiagram.Datatype.ClassInstances()
import Data.Ratio((%))

spec :: Spec
spec
  = do
    describe "enum arrows task" $ do
      context "submission rating" $ do
        it "compute solution" $ do
          enumArrowsSolution defaultEnumInstance
          `shouldBe`
          [(["2"],["c"]),(["5"],["b"]),(["1"],["a"]),(["3"],["d"]),(["4"],["e"])]
        it "no duplicates in answer" $ do
          let
            noDuplicates answer
              = any (\(i,_) -> 1 < length (filter ((==) i . fst) answer)) answer
            (submission::[(String,String)])
              = [("1","1"),("2","2"),("3","3"),("4","4"),("5","5")
                ,("6","6"),("7","7"),("8","8"),("9","9"),("10","10")]
           in
           noDuplicates submission `shouldBe` False
        it "duplicates in answer" $ do
          let
            noDuplicates answer
              = any (\(i,_) -> 1 < length (filter ((==) i . fst) answer)) answer
            (submission::[(String,String)])
              = [("1","1"),("2","2"),("3","3"),("4","4"),("5","5")
                ,("6","6"),("7","7"),("8","8"),("9","9"),("10","10"),("1","1")]
           in
           noDuplicates submission `shouldBe` True
        it "pass rating test" $ do
          let
            task = defaultEnumInstance
            answer = [("2","c"),("5","b"),("1","a"),("3","d"),("4","e")]
            in
            rate (enumArrowsSolution task) answer `shouldBe` 1
        it "fail rating test by one point" $ do
          let
            task = defaultEnumInstance
            answer = [("2","c"),("5","b"),("1","a"),("3","d"),("4","f")]
            in
            rate (enumArrowsSolution task) answer `shouldBe` (4 % 5)
        it "ambiguous enum test variant 1" $ do
          let
            solution
              = [(["1"],["c"]),(["2"],["d"]),(["3"],["b"]),(["4"],["c"])
                ,(["5"],["c"]),(["6"],["e"]),(["7"],["f"]),(["8","9"],["a","b"])]
            submission1
              = [("1","c"),("2","d"),("3","b"),("4","c"),("5","c")
                ,("6","e"),("8","b"),("9","a"),("7","f")]
            in
            rate solution submission1 `shouldBe` 1
        it "ambiguous enum test variant 2" $ do
          let
            solution
              = [(["1"],["c"]),(["2"],["d"]),(["3"],["b"]),(["4"],["c"])
                ,(["5"],["c"]),(["6"],["e"]),(["7"],["f"]),(["8","9"],["a","b"])]
            submission2
              = [("1","c"),("2","d"),("3","b"),("4","c"),("5","c")
                ,("6","e"),("8","a"),("9","b"),("7","f")]
            in
            rate solution submission2 `shouldBe` 1
      context "flatten, submit and rate" $ do
        it "submission test " $ do
          let
            solution
              = [(["1"],["c"]),(["2"],["d"]),(["3"],["b"]),(["4"],["c"])
                ,(["5"],["c"]),(["6"],["e"]),(["7"],["f"]),(["8","9"],["a","b"]),(["10"],["a"])]
            submission
              = [("1","c"),("2","d"),("3","b"),("4","c"),("5","c")
                ,("6","e"),("8","a"),("9","b"),("7","f"),("10","a")]
           in
           rate solution submission `shouldBe` 1
        it "uppercase is not suitable for malforming answer" $ do
          pendingWith "use random to generate bad answer"

