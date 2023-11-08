{-# OPTIONS_GHC -Wno-orphans   #-}

module Main where

import qualified Language.Alloy.Debug as AD (parseInstance)

import qualified Data.ByteString as B (readFile)

import Diagrams.Backend.SVG.CmdLine
import System.Environment (getArgs, withArgs)

import Instance (parseInstance)
import Layout (drawDiagram)
import Style (Styling (Unstyled))

import Control.Monad.Except (runExceptT)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    scope:f:xs' -> do
      inst <- B.readFile f
      alloyInstance <- runExceptT $ AD.parseInstance inst
      let sd = failWith id . parseInstance scope . failWith show $ alloyInstance
      withArgs xs' $ mainWith (drawDiagram Unstyled sd)
    _ -> error "usage: two parameters required: String (scope) FilePath (Alloy instance)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id
