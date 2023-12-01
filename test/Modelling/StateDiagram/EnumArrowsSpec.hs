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

module Modelling.StateDiagram.EnumArrowsSpec (spec) where

import Test.Hspec
import Modelling.StateDiagram.EnumArrows(defaultEnumArrowsConfig
                                        ,defaultEnumInstance
                                        ,enumArrowsSolution
                                        ,enumArrowsEvaluation
                                        ,EnumArrowsInstance(taskSolution, hierarchicalSD)
                                        ,EnumArrowsConfig (shuffle)
                                        ,enumArrowsSyntax
                                        ,rate
                                        ,enumArrows
                                        ,enumArrowsTask
                                        ,enumArrowsFeedback
                                        ,ShufflePolicy(ShuffleLiterals)
                                        ,randomise
                                        )
import System.Directory(createDirectoryIfMissing)
import Modelling.StateDiagram.Datatype.ClassInstances()
import Data.Ratio((%))
import Control.Monad.Output ( Language(English), LangM', ReportT )


import qualified Control.Monad.Output.Generic     as GenericOutput (withLang)
import Data.Maybe                       (fromMaybe)
import Data.Char (toUpper)
import Modelling.StateDiagram.Datatype( unUML
                                      ,Connection(transition,pointFrom, pointTo))
import Data.List(sortBy)

withLang :: LangM' (ReportT (IO ()) IO) a -> Language -> IO a
withLang x l =
  fromMaybe (error "failed")
  <$> GenericOutput.withLang @Language @(ReportT (IO ()) IO) x l

spec :: Spec
spec
  = do
    describe "enumerate" $ do
      it "generate chart and solve it correctly" $ do
        (timestamp::Int) <- pure 1111111111
        createDirectoryIfMissing True ("./session_temp/enumArrows"::FilePath)
        task <- enumArrows defaultEnumArrowsConfig timestamp
        enumArrowsTask ("./session_temp/enumArrows"::FilePath) task `withLang` English
        let sub = concatMap (uncurry zip) $ enumArrowsSolution task
        enumArrowsSyntax task sub `withLang` English
        enumArrowsEvaluation task sub `withLang` English
        enumArrowsFeedback task sub `withLang` English
        rate (taskSolution task) sub `shouldBe` 1

      it "generate chart and solve it incorrectly" $ do
        (timestamp::Int) <- pure 1111111111
        createDirectoryIfMissing True ("./session_temp/enumArrows"::FilePath)
        task <- enumArrows defaultEnumArrowsConfig timestamp
        enumArrowsTask ("./session_temp/enumArrows"::FilePath) task `withLang` English
        let sub = concatMap (uncurry zip) $ enumArrowsSolution task
        let sub' = drop 3 sub
        enumArrowsSyntax task sub `withLang` English
        enumArrowsEvaluation task sub' `withLang` English
        enumArrowsFeedback task sub' `withLang` English
        rate (taskSolution task) sub' `shouldBe` fromIntegral (length sub - 3) % fromIntegral (length sub)

      it "generate chart and solve it entirely wrong" $ do
        (timestamp::Int) <- pure 1111111111
        createDirectoryIfMissing True ("./session_temp/enumArrows"::FilePath)
        task <- enumArrows defaultEnumArrowsConfig timestamp
        enumArrowsTask ("./session_temp/enumArrows"::FilePath) task `withLang` English
        let sub = map (\(x,y) -> (,) x $ map toUpper y) $ concatMap (uncurry zip) $ enumArrowsSolution task
        enumArrowsSyntax task sub `withLang` English
        enumArrowsEvaluation task sub `withLang` English
        enumArrowsFeedback task sub `withLang` English
        rate (taskSolution task) sub `shouldBe` 0

      it "answer test" $ do
        enumArrowsSolution defaultEnumInstance
        `shouldBe`
        [(["2"],["c"]),(["5"],["b"]),(["1"],["a"]),(["3"],["d"]),(["4"],["e"])]
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
      it "shuffle transition labels" $ do
        (timestamp::Int) <- pure 1111111111
        createDirectoryIfMissing True ("./session_temp/enumArrows"::FilePath)
        task <- enumArrows (defaultEnumArrowsConfig { shuffle = Just ShuffleLiterals }) timestamp
        enumArrowsTask ("./session_temp/enumArrows"::FilePath) task `withLang` English
        task' <- randomise task

        let c1 = sortBy (\x y -> compare (transition x) (transition y)) $
                 unUML (\_ _ cons _  -> cons) $ hierarchicalSD task

        let c2 = unUML (\_ _ cons _  -> cons) $ hierarchicalSD task'

        let srcs = zipWith (\x y -> pointFrom x == pointFrom y) c1 c2
        let dsts = zipWith (\x y -> pointTo x == pointTo y) c1 c2

        let epsilon1
              = zipWith (\x y
                          -> (transition x /= "") || (transition y == "")) c1 c2

        let epsilon2
              = zipWith (\x y
                           -> (transition y /= "") || (transition x == "") ) c1 c2

        (c1 `shouldNotBe` c2) *> (and epsilon1 `shouldBe` True)
         *> (and srcs `shouldBe` True) *> (and dsts `shouldBe` True)
         *> (and epsilon2 `shouldBe` True) -- only the transition labels have been shuffled











