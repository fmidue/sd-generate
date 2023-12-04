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
                                         --,randomise
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
    task <- enumArrows defaultEnumArrowsConfig timestamp -- >>= randomise
    print task
    -- visualize task
    enumArrowsTask ("./session_temp/enumArrows"::FilePath) task `withLang` English
    putStrLn ("\n" ++ "cheat solution:" ++ show (concatMap (uncurry zip) $ taskSolution task))
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
    -- user syntax checking function
    enumArrowsSyntax task sub `withLang` English
    -- task submission evaluation function
    points <- enumArrowsEvaluation task sub `withLang` English
    -- user result
    print points
    return ()

