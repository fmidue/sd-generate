{-# LANGUAGE OverloadedStrings #-}

{- |
Checkers checking the drawability of 'UMLStateDiagrams'.
-}
module Checkers.Drawability (
  checkDrawability,
  ) where

import qualified Data.ByteString.Lazy as LBS (length)

import Datatype                         (UMLStateDiagram)
import Layout                           (drawDiagram)

import Control.Exception                (SomeException, try, evaluate)
import Data.Either                      (fromRight)
import Diagrams.Backend.SVG             (SVG (SVG), Options (SVGOptions))
import Diagrams.Prelude                 (renderDia)
import Diagrams.TwoD.Size               (mkWidth)
import Graphics.Svg.Core                (renderBS)
import System.IO.Unsafe                 (unsafePerformIO)

{- |
Tries to draw the 'UMLStateDiagram' by performing the drawing algorithm.
Returns 'True' if no exception occurs.
-}
checkDrawability :: UMLStateDiagram -> Maybe String
checkDrawability x = unsafePerformIO $ recover <$> try (evaluate $ draws x)
  where
    recover :: Either SomeException Bool -> Maybe String
    recover e = if fromRight False e then Nothing else Just "drawDiagram crashed"
    draws :: UMLStateDiagram -> Bool
    draws = (>= 0) . LBS.length . renderBS
      . renderDia SVG (SVGOptions (mkWidth 500) Nothing "" [] True)
      . drawDiagram
