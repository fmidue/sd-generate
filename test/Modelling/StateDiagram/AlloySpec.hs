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

module Modelling.StateDiagram.AlloySpec where

import Test.Hspec(Spec
                 ,describe
                 ,it
                 ,shouldBe
                 ,shouldNotBe)
import Language.Alloy.Call(getInstances)
import Data.String(IsString, fromString)
import System.Directory(createDirectoryIfMissing)
import Modelling.StateDiagram.Datatype.ClassInstances()
import Modelling.StateDiagram.Config(defaultSDConfig
                                    ,sdConfigToAlloy)
import Modelling.StateDiagram.Instance(parseInstance)
import Modelling.StateDiagram.Datatype(UMLStateDiagram)
import Modelling.StateDiagram.Checkers
    ( checkCrossings,
      checkDrawability,
      checkEndState,
      checkForkAndJoin,
      checkHistory,
      checkNameUniqueness,
      checkRepresentation,
      checkSemantics,
      checkStructure,
      checkUniqueness )
import Data.Maybe(isNothing)

spec :: Spec
spec
  = do
    describe "implementation test collection for StateDiagram tasks" $
      do
      it "print default alloy sd config to \"./temp/sd.als\"" $
        do
        createDirectoryIfMissing True "./temp"
        writeFile "./temp/sd.als" $ sdConfigToAlloy defaultSDConfig
        return ()
      it "parsing an Alloy chart instance yields some result" $
        do
        inst <- getInstances (Just 500) (sdConfigToAlloy defaultSDConfig)
        let chart = map (failWith id . parseInstance "this") inst !! 499
        show chart `shouldNotBe` ""
      it "default Alloy sd config wont return empty chart instances (as checkers might still pass on [])" $
        do
        inst <- getInstances (Just 500) (sdConfigToAlloy defaultSDConfig)
        length inst `shouldBe` 500
      it "retrieving default Alloy SD chart instances does satisfy the Haskell chart checkers" $
        do
        inst <- getInstances (Just 500) (sdConfigToAlloy defaultSDConfig)
        and (concatMap (zipWith (\ checker chart' -> isNothing (checker chart')) (map snd checkers) . repeat . failWith id . parseInstance "this") inst) `shouldBe` True
      -- it "config checker denies unreasonable requests" $

instance IsString a => MonadFail (Either a) where
  fail = Left . fromString

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

-- todo: use the version from Checkers module and filter the removed test out by "str" /= fst .
checkers :: [(String, UMLStateDiagram String Int -> Maybe String)]
checkers =
  [ ("checkRepresentation", checkRepresentation)
  , ("checkStructure", checkStructure)
  , ("checkCrossings", checkCrossings)
  , ("checkNameUniqueness", checkNameUniqueness)
  , ("checkUniqueness", checkUniqueness)
  , ("checkEndState", checkEndState)
  , ("checkForkAndJoin", checkForkAndJoin)
  , ("checkHistory", checkHistory)
  , ("checkSemantics", checkSemantics)
  , ("checkDrawability", checkDrawability)
  ]
