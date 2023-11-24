import Control.Monad.Output             (Language (English))

import Common                           (withLang)

import Data.Time.Clock.POSIX(getPOSIXTime)
import System.Directory(createDirectoryIfMissing)
import Modelling.StateDiagram.EnumArrows (enumArrowsTask
                                         ,enumArrowsSyntax
                                         ,enumArrowsEvaluation
                                         ,defaultEnumArrowsConfig
                                         ,enumArrowsSolution
                                         ,enumArrows)

-- run with: stack run enumeratedArrowsDemo --
main :: IO ()
main
  = do
    (timestamp::Int) <- round <$> getPOSIXTime
    createDirectoryIfMissing True ("./session_temp/enumArrows"::FilePath)
    putStrLn $ "Seed: " ++ show timestamp
    -- initialize Alloy and instance selector
    taskEnv <- enumArrows defaultEnumArrowsConfig timestamp
    -- pick a concrete instance
    task <- enumArrowsTask ("./session_temp/enumArrows"::FilePath) taskEnv `withLang` English
    -- visualize task
    print task
    putStrLn ("\ncheat solution:" ++ show (enumArrowsSolution taskEnv))
    -- user response (task assignment -> solution submission)
    subm <- fmap read getLine
    -- user syntax checking function
    enumArrowsSyntax taskEnv subm `withLang` English
    -- task submission evaluation function
    points <- enumArrowsEvaluation taskEnv subm `withLang` English
    -- user result
    print points
    return ()

