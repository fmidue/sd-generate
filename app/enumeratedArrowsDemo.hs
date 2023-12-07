import Control.Monad.Output             (Language (English))

import Common                           (withLang)

import Data.Time.Clock.POSIX(getPOSIXTime)
import System.Directory(createDirectoryIfMissing
                       ,renameFile)
import Modelling.StateDiagram.EnumArrows (enumArrowsTask
                                         ,enumArrowsSyntax
                                         ,enumArrowsEvaluation
                                         ,defaultEnumArrowsConfig
                                         ,checkEnumArrowsConfig
                                         ,EnumArrowsInstance(taskSolution)
                                         ,enumArrows
                                         , randomise, randomiseLayout, enumArrowsFeedback

                                         )
import Data.Foldable(forM_)

-- run with: stack run enumeratedArrowsDemo --
main :: IO ()
main
  = do
    -- check the task configuration
    forM_ (checkEnumArrowsConfig defaultEnumArrowsConfig) error
    putStrLn "configuration looking good"
    (timestamp::Int) <- round <$> getPOSIXTime
    createDirectoryIfMissing True ("./session_temp/enumArrows"::FilePath)
    putStrLn $ "Seed: " ++ show timestamp
    -- initialize Alloy and instance selector

    -- and pick a concrete instance, and optionally randomise triggers and names
    task <- enumArrows defaultEnumArrowsConfig timestamp >>= randomise >>= randomiseLayout
    -- visualize task
    enumArrowsTask ("./session_temp/enumArrows"::FilePath) task `withLang` English
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
    writeFile ("./session_temp/enumArrows/" ++ show timestamp ++ "_generatorConfig.txt") (show defaultEnumArrowsConfig)

    -- user submission
    sub <- fmap read getLine
    -- user submission syntax checking
    enumArrowsSyntax task sub `withLang` English
    -- task submission evaluation function
    points <- enumArrowsEvaluation task sub `withLang` English
    -- submission rating
    print points
    -- extended submission feedback
    enumArrowsFeedback task sub `withLang` English
    return ()

