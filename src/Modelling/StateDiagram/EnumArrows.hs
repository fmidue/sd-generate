
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Modelling.StateDiagram.EnumArrows (enumArrowsTask
                                         ,enumArrowsSolution
                                         ,correctEnumeration
                                         ,enumArrowsInstance
                                         ,enumArrowsSyntax
                                         ,enumArrowsEvaluation
                                         ,EnumArrowsInstance(..)
                                         ,EnumArrowsConfig(..)
                                         ,defaultEnumArrowsConfig
                                         ,defaultEnumInstance
                                         ,enumArrows
                                         ,enumArrowsInstanceCheck
                                         ,rate
                                         ,enumArrowsFeedback
                                         ,checkEnumArrowsConfig)
where

{-
brief task description:

given a flattened state chart, where the transition literals have been replaced by integers,
ask the user to supply a list of tuples, where the first element is the integer of the transition
and the second element is the transition literal as string that is supposed to be at that place,
for all enumerated arrows of the chart.
-}

import Modelling.StateDiagram.Datatype
    ( StateDiagram(StateDiagram
                  ,label
                  ,name
                  ,substates
                  ,connections
                  ,startState),
      UMLStateDiagram,
      Connection(transition, pointFrom, pointTo),
      unUML,
      umlStateDiagram,
      rename )
import Modelling.StateDiagram.Config(SDConfig
                                    ,sdConfigToAlloy
                                    ,defaultSDConfig, noEmptyTriggers, hierarchicalStates, chartLimits)
import Modelling.StateDiagram.Alloy()
import Modelling.StateDiagram.PlantUMLDiagrams(drawSDToFile)
import System.FilePath(combine)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  Rated,
  ($=<<),
  english,
  translate,
  printSolutionAndAssert
  )
import Control.Monad.Random (
  MonadRandom,
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Language.Alloy.Call (getInstances)
import Modelling.StateDiagram.Instance (parseInstance
                                       ,failWith)
import Modelling.StateDiagram.Flatten (flatten)
import Data.List(groupBy
                ,nub
                ,singleton
                ,sortBy
                ,find)
import Modelling.StateDiagram.Example (flatCase1)
import Control.Monad.Random.Lazy (randomRIO)
import Data.Either (rights
                   ,lefts)

data EnumArrowsInstance
  = EnumArrowsInstance {
        hierarchicalSD :: UMLStateDiagram String Int
      , flatAndEnumeratedSD :: UMLStateDiagram String Int
      , taskSolution :: [([Int], [String])]
    }

data RenamingStrategy
  = HierarchicalConcatenation
  | JustTheInnermostName
  deriving (Show)

data EnumArrowsConfig
  = EnumArrowsConfig {
      sdConfig :: SDConfig
    , maxInstances :: Maybe Integer
    , syntaxWarnTooManyArrows :: Bool
    , printExtendedFeedback :: Bool
    , renamingStrategy :: RenamingStrategy
  } deriving (Show)

defaultEnumArrowsConfig :: EnumArrowsConfig
defaultEnumArrowsConfig
  = EnumArrowsConfig {
      sdConfig = defaultSDConfig
    , maxInstances = Just 1000
    , printExtendedFeedback = False
    , syntaxWarnTooManyArrows = False
    , renamingStrategy = JustTheInnermostName
  }

enumArrows :: MonadIO m => EnumArrowsConfig -> Int -> m EnumArrowsInstance
enumArrows config timestamp
  = evalRandT (enumArrowsInstance config) (mkStdGen timestamp)

enumArrowsTask :: (OutputMonad m, MonadIO m) => FilePath -> EnumArrowsInstance -> LangM m
enumArrowsTask path task
  = do
    paragraph $ translate $ do
      english "Consider the following state chart."
    image $=<< liftIO $ drawSDToFile (combine path "plain") (hierarchicalSD task)
    paragraph $ translate $ do
      english "Which was flattened, having the transition literals replaced by integers."
    image $=<< liftIO $ drawSDToFile (combine path "flattened") (flatAndEnumeratedSD task)
    paragraph $ translate $ do
      english "Please supply a list of tuples, where the first element is the integer label of the transition\n\
               \ and the second element is a transition literal string, that is supposed\
               \ to be at that place."
    paragraph $ translate $ do
      english "You may use the following syntax to denote the missing arrows:\n\
               \ [(1,\"a\")] is a list, referring to a single transition labelled (1) that is supposed to be the literal 'a'."
    pure ()

enumArrowsInstance :: (RandomGen g, MonadIO m) => EnumArrowsConfig -> RandT g m EnumArrowsInstance
enumArrowsInstance (EnumArrowsConfig sdConfig (Just maxInstances)  _ _ renamingStrategy)
  = do
    inst <- liftIO $ getInstances (Just maxInstances) (sdConfigToAlloy sdConfig)
    r <- liftIO (randomRIO (0, fromIntegral maxInstances - 1) :: IO Int)
    let chart = map (failWith id . parseInstance "this") inst !! r
    let flatChart
          = rename (case renamingStrategy of
                                  HierarchicalConcatenation
                                    -> concat
                                  JustTheInnermostName
                                    -> last
                   ) (flatten chart)
    pure EnumArrowsInstance {
           hierarchicalSD = chart
         , taskSolution
             = correctEnumeration flatChart
         , flatAndEnumeratedSD
             = umlStateDiagram $
               unUML (\name substates connection startState
                         -> StateDiagram {
                              name = name
                            , substates = substates
                            , connections
                                = zipWith (\c l
                                               -> c {transition = (show::Int -> String) l})
                                  connection [1..]
                            , startState = startState
                            , label = 999
                            }
                     ) flatChart
         }
enumArrowsInstance _ = undefined

enumArrowsInstanceCheck :: (MonadIO m, MonadRandom m) => EnumArrowsConfig -> EnumArrowsInstance -> m (Maybe String)
enumArrowsInstanceCheck _ task
  | length (enumArrowsSolution task) > 30
    = return $ Just "The solution chart exceeds a reasonable amount of transitions, it would be tedious to enumerate them all."
  | otherwise = return Nothing

checkEnumArrowsConfig :: EnumArrowsConfig -> Maybe String
checkEnumArrowsConfig taskConfig
  | not (noEmptyTriggers (sdConfig taskConfig))
  = Just "The chart may contain empty triggers, which are not allowed in this task setting."
  | fst (hierarchicalStates (chartLimits (sdConfig taskConfig))) < 1
  = Just "The chart must have at least one hierarchical state."
  | otherwise = Nothing

enumArrowsSyntax :: (OutputMonad m) => EnumArrowsInstance -> [(Int,String)] -> LangM m
enumArrowsSyntax task answer
  = do
    assertion (any (\(i,_) -> 1 < length (filter ((==) i . fst) answer)) answer) $ translate $ do
      english "No literal was assigned more than once."
    assertion (any (\(i,_) -> i < 1) answer) $ translate $ do
      english "Transition enumeration must be positive integers."
    assertion ( syntaxWarnTooManyArrows defaultEnumArrowsConfig &&
                length answer > (length (enumArrowsSolution task) +
               (length (enumArrowsSolution task) `div` 2))) $ translate $ do
      english "Transition enumeration must not exceed the number of transitions in the chart."
    assertion (any (\(_,l) -> l == "") answer) $ translate $ do
      english "Transition literals must not be empty."
    assertion (null answer) $ translate $ do
      english "No empty list of integer to string tuples was supplied."
    return ()

enumArrowsEvaluation :: (OutputMonad m) => EnumArrowsInstance -> [(Int,String)] -> Rated m
enumArrowsEvaluation task answer
  = printSolutionAndAssert
    (Just $ show (concatMap (uncurry zip) $ enumArrowsSolution task))
    (rate (enumArrowsSolution task) answer)

enumArrowsSolution :: EnumArrowsInstance -> [([Int], [String])]
enumArrowsSolution EnumArrowsInstance {taskSolution}
  = taskSolution

correctEnumeration :: UMLStateDiagram String Int -> [([Int], [String])]
correctEnumeration
  = unUML (\_ _ connection _
             -> map (\x
                       -> (,)
                          (concatMap (singleton . fst) x)
                          (concatMap (singleton . transition . snd) x))
                $
                groupBy (\(_,x) (_,y)
                            -> pointFrom x == pointFrom y &&
                               pointTo x == pointTo y)
                $
                sortBy (\(_,x) (_,y)
                          -> compare (pointFrom x, pointTo x)
                                     (pointFrom y, pointTo y))
                $
                zip [1..] connection
    )

-- we must assert before calling this function that every label
-- is only used once in the submission; for all (i1,_) (i2,_) => i1 /= i2
rate :: [([Int], [String])] -> [(Int,String)] -> Rational
rate solution submission
  = let
    answers
      = map (\(i,l)
           -> maybe (Left $ (,) i l)
                    (\(_,ls)
                        -> if l `elem` ls
                           then Right $ (,) i l
                           else Left $ (,) i l)
                    (find (elem i . fst) solution)
           ) (nub submission)
    correct = sum $ map (length . snd) (rights answers)
    total = sum $ map (length . snd) solution
    in
    (toRational correct / toRational total)


enumArrowsFeedback :: (OutputMonad m) => EnumArrowsInstance -> [(Int,String)] -> LangM m
enumArrowsFeedback task submission
  = let
    solution = enumArrowsSolution task
    answers
      = map (\(i,l)
           -> maybe (Left $ (,) i l)
                    (\(_,ls)
                        -> if l `elem` ls
                           then Right $ (,) i l
                           else Left $ (,) i l)
                    (find (elem i . fst) solution)
           ) (nub submission)
    missing
      = concatMap (uncurry zip) $
        filter (\(_,ls) -> ls /= []) $
        foldl (\solution' (i,l)
                 -> map (\(is,ls)
                           -> if i `elem` is
                              then (,) is (filter (/= l) ls)
                              else (is,ls)
                        ) solution'
              ) solution submission
    in
    do
    paragraph $ translate $ do
      english ("correct: " ++ show (rights answers) ++ "\n" ++
               "wrong: " ++ show (lefts answers) ++ "\n" ++
               "missing: " ++ show missing ++ "\n")
    return ()


defaultEnumInstance :: EnumArrowsInstance
defaultEnumInstance
  = EnumArrowsInstance {
    hierarchicalSD = flatCase1
  , flatAndEnumeratedSD
      = umlStateDiagram $
         unUML (\name substates connection startState
                  -> StateDiagram {
                                    name = name
                                  , substates = substates
                                  , connections
                                      = zipWith (\c l
                                                     -> c {transition = (show::Int -> String) l})
                                        connection [1..]
                                  , startState = startState
                                  , label = 999
                                  }
               ) (rename last (flatten flatCase1))
  , taskSolution
      = correctEnumeration
        (rename last $ flatten flatCase1)
  }
