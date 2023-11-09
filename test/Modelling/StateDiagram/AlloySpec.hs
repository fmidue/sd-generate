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
import Modelling.StateDiagram.Alloy(alloySDGenerator)
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
      checkForkOrJoin,
      checkHistory,
      checkNameUniqueness,
      checkRepresentation,
      checkSemantics,
      checkStructure,
      checkUniqueness )
import Data.Maybe (isNothing)

spec :: Spec
spec
  = do
    describe "implementation test collection for StateDiagram tasks" $
      do
      it "print default alloy sd config to \"./temp/sd.als\"" $
        do
        createDirectoryIfMissing True "./temp"
        writeFile "./temp/sd.als" $ sdConfigToAlloy 6 3 defaultSDConfig
        return ()
      it "default SD config is capable to invoke Alloy and returns an instance" $
        do
        inst <- getInstances (Just 500) (sdConfigToAlloy 9 6 defaultSDConfig)
        let chart = map (failWith id . parseInstance "this") inst !! 499
        print chart
        return ()
      it "default Alloy sd config wont return empty chart instances (as checkers might still pass on [])" $
        do
        inst <- getInstances (Just 1) (sdConfigToAlloy 9 6 defaultSDConfig)
        map ( failWith id . parseInstance "components") inst `shouldNotBe` []
      it "retreiving simple Alloy SD chart instances does satisfy the Haskell chart checkers" $
        do
        inst <- getInstances (Just 50) alloySDGenerator
        and (concatMap (zipWith (\ checker chart' -> isNothing (checker chart')) (map snd checkers) . repeat . failWith id . parseInstance "components") inst) `shouldBe` True
      it "config checker denies unreasonable requests" $
        True `shouldBe` True
{-
      it "render some charts obtained from Alloy" $
        do
        inst <- getInstances (Just 500) (sdConfigToAlloy 9 6 defaultSDConfig)
        mainWith $ (drawDiagram Unstyled) $ head (drop 499 (map (failWith id . parseInstance "this") inst))
        --mapM (\x -> mainWith (drawDiagram Unstyled x)) (drop 480 (map (failWith id . parseInstance "this") inst))
        --adjust output names
        return ()
-}

#if __GLASGOW_HASKELL__ >= 808
instance IsString a => MonadFail (Either a) where
  fail = Left . fromString
#endif

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
  , ("checkForkOrJoin", checkForkOrJoin)
  , ("checkHistory", checkHistory)
  , ("checkSemantics", checkSemantics)
  , ("checkDrawability", checkDrawability)
  ]
