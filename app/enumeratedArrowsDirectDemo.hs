{-# LANGUAGE FlexibleContexts #-}
module Main (main) where
import Control.OutputCapable.Blocks (
  LangM,
  Language (English),
  Rated,
  ReportT,
  )
import Control.OutputCapable.Blocks.Generic     (($>>=))
import Control.OutputCapable.Blocks.Debug       (testTask)
import Data.Functor                     (($>))
import Data.Time.Clock.POSIX(getPOSIXTime)
import System.Directory(createDirectoryIfMissing
                       ,renameFile)
import Modelling.StateDiagram.EnumArrows ( enumArrowsTask
                                         , enumArrowsSyntax
                                         , enumArrowsEvaluation
                                         , defaultEnumArrowsInstance
                                         , checkEnumArrowsInstance
                                         , EnumArrowsInstance(taskSolution)
                                         , randomise
                                         , randomiseLayout
                                         , enumArrowsFeedback
                                         )
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

-- run with: stack run enumeratedArrowsDirectDemo --
main :: IO ()
main = do
  t <- getSeed
  testTask Nothing English (generate t) (describe t) partial full submission
  where
    getSeed :: IO Int
    getSeed = round <$> getPOSIXTime
    generate :: Int -> IO EnumArrowsInstance
    generate timestamp = do
      hSetBuffering stdout NoBuffering
      -- check the task instance
      mapM_ error $ checkEnumArrowsInstance defaultEnumArrowsInstance
      putStrLn "instance looking good"
      createDirectoryIfMissing True ("./session_temp/enumArrows"::FilePath)
      putStrLn $ "Seed: " ++ show timestamp
      -- initialize Alloy and instance selector

      -- on the concrete instance, optionally randomise triggers and names
      randomise defaultEnumArrowsInstance
        >>= randomiseLayout
      -- visualize task
    describe :: Int -> EnumArrowsInstance -> LangM (ReportT (IO ()) IO)
    describe timestamp task =
      enumArrowsTask ("./session_temp/enumArrows"::FilePath) task
      $>>= \x -> (*> pure x) $ pure $ do
        print task
        putStrLn ("\n" ++ "cheat solution: " ++ show (concatMap (uncurry zip) $ taskSolution task))
        -- user response (task assignment -> solution submission)

        -- rename the session files and store the solution
        putStrLn ("the .svg files have been renamed to include the seed: " ++ show timestamp)
        renameFile ("./session_temp/enumArrows" ++ "/plainDiagram.svg")
                   ("./session_temp/enumArrows/" ++ show timestamp ++ "_plainDiagram.svg")
        renameFile ("./session_temp/enumArrows" ++ "/flattenedDiagram.svg")
                   ("./session_temp/enumArrows/" ++ show timestamp ++ "_flattenedDiagram.svg")
        writeFile ("./session_temp/enumArrows/" ++ show timestamp ++ "_solution.txt") (show (taskSolution task))
        writeFile ("./session_temp/enumArrows/" ++ show timestamp ++ "_usedInstance.txt") (show defaultEnumArrowsInstance)
    submission =
      -- user submission
      fmap read getLine
    partial =
      -- user submission syntax checking
      enumArrowsSyntax
    full :: EnumArrowsInstance -> [(String, String)] -> Rated (ReportT (IO ()) IO)
    full task sub =
      -- task submission evaluation function
      enumArrowsEvaluation task sub
      -- extended submission feedback
      $>>= (enumArrowsFeedback task sub $>)
