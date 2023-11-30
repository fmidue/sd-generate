import Control.Monad.Output             (Language (English))

import Common                           (withLang)

import Data.Time.Clock.POSIX(getPOSIXTime)
import System.Directory(createDirectoryIfMissing
                       ,renameFile)
import Modelling.StateDiagram.EnumArrows (enumArrowsTask
                                         ,enumArrowsSyntax
                                         ,enumArrowsEvaluation
                                         ,defaultEnumArrowsConfig
                                         ,EnumArrowsInstance(taskSolution)
                                         ,enumArrows)

-- run with: stack run enumeratedArrowsDemo --
main :: IO ()
main
  = do
    (timestamp::Int) <- round <$> getPOSIXTime
    createDirectoryIfMissing True ("./session_temp/enumArrows"::FilePath)
    putStrLn $ "Seed: " ++ show timestamp
    -- initialize Alloy and instance selector
    -- and pick a concrete instance
    task <- enumArrows defaultEnumArrowsConfig timestamp
    -- visualize task
    enumArrowsTask ("./session_temp/enumArrows"::FilePath) task `withLang` English
    putStrLn ("\n" ++ "cheat solution:" ++ show (taskSolution task))
    -- user response (task assignment -> solution submission)

    -- rename the session files and store the solution
    putStrLn ("the .svg files have been renamed to include the seed: " ++ show timestamp)
    renameFile (("./session_temp/enumArrows" ++ "/plainDiagram.svg")::FilePath)
               (("./session_temp/enumArrows/" ++ show timestamp ++ "_plainDiagram.svg")::FilePath)
    renameFile (("./session_temp/enumArrows" ++ "/flattenedDiagram.svg")::FilePath)
               (("./session_temp/enumArrows/" ++ show timestamp ++ "_flattenedDiagram.svg")::FilePath)
    writeFile (("./session_temp/enumArrows/" ++ show timestamp ++ "_solution.txt")::FilePath) (show (taskSolution task))
    writeFile (("./session_temp/enumArrows/" ++ show timestamp ++ "_generatorConfig.txt")::FilePath) (show defaultEnumArrowsConfig)

    sub <- fmap read getLine
    -- user syntax checking function
    enumArrowsSyntax task sub `withLang` English
    -- task submission evaluation function
    points <- enumArrowsEvaluation task sub `withLang` English
    -- user result
    print points
    return ()
