{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Modelling.StateDiagram.EnumArrowsSpec (spec)
where

import Test.Hspec (shouldBe
                  ,shouldNotBe
                  ,it
                  ,describe
                  ,Spec
                  ,context)
import Modelling.StateDiagram.EnumArrows(defaultEnumInstance
                                        ,enumArrowsSolution
                                        ,rate
                                        ,EnumArrowsInstance (taskSolution
                                                            ,randomization
                                                            ,shuffle
                                                            ,flatAndEnumeratedSD)
                                        ,randomiseLayout
                                        ,randomise
                                        ,ShufflePolicy (..)
                                        ,hierarchicalSD
                                        ,defaultEnumArrowsConfig
                                        ,enumArrows
                                        )

import Modelling.StateDiagram.Datatype.ClassInstances()
import Data.Bifunctor(first)
import Modelling.StateDiagram.Datatype (Connection(..)
                                       ,globalise
                                       ,UMLStateDiagram (unUML')
                                       ,StateDiagram (connections)
                                       ,collectNames)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Char (toUpper)
import Data.Ratio ((%))


spec :: Spec
spec
  = do
    describe "enum arrows task" $ do
      context "randomization tests" $ do

        it "randomize chart layout for default chart" $ do
          let task = defaultEnumInstance { randomization = True }
          task' <- randomiseLayout task
          task `shouldNotBe` task'

        it "test shuffle transition labels" $ do
          let task = defaultEnumInstance { shuffle = Just ShuffleTriggers }
          task' <- randomise task
          let hsc = connections . unUML' . globalise $ hierarchicalSD task
          let hsc' = connections . unUML' . globalise $ hierarchicalSD task'
          let transitions x y
                = [ (t,t') | Connection{ pointFrom = pF
                                        , pointTo = pT
                                        , transition = t } <- x
                            , Connection{ pointFrom=pF'
                                        , pointTo=pT'
                                        , transition=t' } <- y
                            , pF == pF' && pT == pT' ]
          let shuffledHsc = transitions hsc hsc'
          (hsc `shouldNotBe` hsc')
           <* (length shuffledHsc `shouldBe` length hsc)
           <* (length shuffledHsc `shouldBe` length hsc')
           <* (any (uncurry (/=)) shuffledHsc `shouldBe` True)
           <* (all ((\t -> t `elem` map transition hsc) . snd) shuffledHsc `shouldBe` True)
           <* shouldNotBe [] hsc
           <* shouldNotBe [] hsc'

        it "shuffle state names" $ do
          let task = defaultEnumInstance { shuffle = Just ShuffleNames }
          task' <- randomise task
          (collectNames . hierarchicalSD $ task) `shouldNotBe` (collectNames . hierarchicalSD $ task')

        it "shuffle state names and transition labels" $ do
          let task = defaultEnumInstance { shuffle = Just ShuffleNamesAndTriggers }
          task' <- randomise task
          (flatAndEnumeratedSD task `shouldNotBe` flatAndEnumeratedSD task')
            *> (hierarchicalSD task `shouldNotBe` hierarchicalSD task')
            *> (taskSolution task `shouldNotBe` taskSolution task')

      context "submission rating" $ do
        it "no duplicates in answer" $ do
          let
            noDuplicates answer
              = any (\(i,_) -> 1 < length (filter ((==) i . fst) answer)) answer
            (submission::[(String,String)])
              =  map (\ x_ -> curry (first show) x_ undefined) (take 10 ([1 .. ]::[Int]))
           in
           noDuplicates submission `shouldBe` False
        it "duplicates in answer" $ do
          let
            noDuplicates answer
              = any (\(i,_) -> 1 < length (filter ((==) i . fst) answer)) answer
            (submission::[(String,String)])
              = map (\ x_ -> curry (first show) x_ undefined) (take 10 ([1 .. ]::[Int]))
                ++ [("1",undefined)]
           in
           noDuplicates submission `shouldBe` True
        it "pass rating test" $ do
          let
            task = defaultEnumInstance
            answer = concatMap (uncurry zip) $ taskSolution task
            in
            rate (enumArrowsSolution task) answer `shouldBe` 1
        it "fail rating test by one point" $ do
          let
            task = defaultEnumInstance
            answer = concatMap (uncurry zip) $ taskSolution task
            in
            rate (enumArrowsSolution task) (tail answer)
            `shouldBe`
            toRational (length answer - 1) / toRational (length answer)
        it "ambiguous enum test variant 1" $ do
          let
            solution
              = [(["5"],["c"]),(["6"],["e"]),(["7"],["f"]),(["8","9"],["a","b"])]
            submission1
              = [("5","c"),("6","e"),("8","b"),("9","a"),("7","f")]
            in
            rate solution submission1 `shouldBe` 1
        it "ambiguous enum test variant 2" $ do
          let
            solution
              = [(["5"],["c"]),(["6"],["e"]),(["7"],["f"]),(["8","9"],["a","b"])]
            submission2
              = [("5","c"),("6","e"),("8","a"),("9","b"),("7","f")]
            in
            rate solution submission2 `shouldBe` 1
      context "generate and solve" $ do
        it "generate chart and solve it correctly" $ do
          (timestamp::Int) <- round <$> getPOSIXTime
          task <- enumArrows defaultEnumArrowsConfig timestamp
          let sub = concatMap (uncurry zip) $ enumArrowsSolution task
          rate (taskSolution task) sub `shouldBe` 1

        it "generate chart and solve it partially" $ do
          (timestamp::Int) <- round <$> getPOSIXTime
          task <- enumArrows defaultEnumArrowsConfig timestamp
          let sub = concatMap (uncurry zip) $ enumArrowsSolution task
          let sub'
                = if even $ length sub
                  then drop (length sub `div` 2) sub
                  else drop (length sub `div` 3) sub
          rate (taskSolution task) sub' `shouldBe` fromIntegral (length sub') % fromIntegral (length sub)

        it "generate chart and solve it entirely wrong" $ do
          (timestamp::Int) <- round <$> getPOSIXTime
          task <- enumArrows defaultEnumArrowsConfig timestamp
          let sub = map (\(x,y) -> (,) x $ map toUpper y) $ concatMap (uncurry zip) $ enumArrowsSolution task
          rate (taskSolution task) sub `shouldBe` 0
