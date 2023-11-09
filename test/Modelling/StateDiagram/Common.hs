-- | This module provides functions required by other testing modules.

module Modelling.StateDiagram.Common (
  withUnitTests,
  ) where

import Control.Monad                    (forM_)
import Data.List                        (isPrefixOf, sort)
import Data.List.Extra                  (replace)
import System.Directory                 (getDirectoryContents)
import System.FilePath                  ((</>), (-<.>))
import Test.Hspec                       (Expectation, Spec, describe, it, runIO)

{-|
Executes unit test cases read from file using results read from file.
-}
withUnitTests
  :: String
  -- ^ file prefix and description of the test case
  -> String
  -- ^ what the test case should do
  -> FilePath
  -- ^ where to find the files
  -> String
  -- ^ the file extension of the expected result files
  -> (String -> String -> Expectation)
  -- ^ How to assert input and expected result
  -> Spec
withUnitTests name does dir extension assertWith = describe name $ do
  fs <- runIO $ sort <$> getDirectoryContents dir
  let testName = name ++ "Test"
  forM_ (filter (testName `isPrefixOf`) fs) $ \fileName -> do
    let file = dir </> fileName
    input <- runIO $ readFile file
    let resultFile = replace "Test" "Result" file -<.> extension
    expectedResult <- runIO $ readFile resultFile
    it (does ++ " for " ++ file) $ assertWith input expectedResult
