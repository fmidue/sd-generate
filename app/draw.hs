module Main where

import qualified Language.Alloy.Debug as AD (parseInstance)

import qualified Data.ByteString as B (readFile)

import Diagrams.Backend.SVG.CmdLine
import System.Environment (getArgs, withArgs)

import Instance (parseInstance)
import Layout (drawDiagram)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    scope:f:xs' -> do
      inst <- B.readFile f
      let sd = failWith id . parseInstance scope . failWith show
            $ AD.parseInstance inst
      withArgs xs' $ mainWith (drawDiagram sd)
    _ -> error "usage: two parameters required: String (scope) FilePath (Alloy instance)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id
