{-# OPTIONS_GHC -Wno-orphans   #-}

module Main where

import qualified Language.Alloy.Debug as AD (parseInstance)

import qualified Data.ByteString as B (readFile)

import Diagrams.Backend.SVG.CmdLine
import System.Environment (getArgs, withArgs)

import Instance (parseInstance)
import Layout (drawDiagram)
import Style (Styling (Unstyled))

import Control.Monad.IO.Class (MonadIO (liftIO))

import Text.Trifecta.Result (ErrInfo)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    scope:f:xs' -> do
      inst <- B.readFile f
      let sd = failWith id . parseInstance scope . failWith show
            $ AD.parseInstance inst
      withArgs xs' $ mainWith (drawDiagram Unstyled sd)
    _ -> error "usage: two parameters required: String (scope) FilePath (Alloy instance)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id


instance MonadIO (Either Text.Trifecta.Result.ErrInfo) where
  liftIO = error "liftIO not implemented for Either Text.Trifecta.Result.ErrInfo"
