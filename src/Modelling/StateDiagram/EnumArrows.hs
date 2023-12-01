
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}

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
                                         ,checkEnumArrowsConfig
                                         ,randomise
                                         ,ShufflePolicy(..))
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
                  ,startState, InnerMostState),
      UMLStateDiagram,
      Connection(transition, pointFrom, pointTo),
      unUML,
      umlStateDiagram,
      rename, globalise )
import Modelling.StateDiagram.Config(SDConfig
                                    ,sdConfigToAlloy
                                    ,defaultSDConfig, noEmptyTriggers, hierarchicalStates, chartLimits)
import Modelling.StateDiagram.Alloy()
import Modelling.StateDiagram.PlantUMLDiagrams
  (drawSDToFile
  ,checkDrawabilityPlantUML)
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
                ,find, partition)
import Modelling.StateDiagram.Example (flatCase1)
import Control.Monad.Random.Lazy (randomRIO)
import Data.Either (rights
                   ,lefts)
import Control.Monad.Loops (iterateUntil)
import Modelling.StateDiagram.Checkers (checkDrawability)
import Data.Maybe (isNothing, fromMaybe)
import Modelling.StateDiagram.Layout (drawDiagram)
import Modelling.StateDiagram.Style (Styling(Unstyled))
import Diagrams.Backend.SVG (renderSVG)
import Diagrams (dims, V2 (V2))
import System.Random.Shuffle (shuffleM)

import Modelling.Auxiliary.Common
import Data.List.Extra (notNull)

data EnumArrowsInstance
  = EnumArrowsInstance {
        hierarchicalSD :: UMLStateDiagram String Int
      , flatAndEnumeratedSD :: UMLStateDiagram String Int
      , taskSolution :: [([String], [String])]
      , chartRenderer :: Renderer
      , shufflePolicy :: ShufflePolicy
      , renamingPolicy :: RenamingStrategy
   } deriving (Show)

data ShufflePolicy
  = ShuffleNamesAndTriggers
  | ShuffleNames
  | ShuffleTriggers
  | DoNotShuffle
  deriving (Show)

data RenamingStrategy
  = HierarchicalConcatenation
  | JustTheInnermostName
  deriving (Show)

data RenderPath
  = RenderPath {
      renderPolicy :: RenderPolicy
    , renderer :: Renderer
  } deriving (Show)

data RenderPolicy
  = RegenerateOnFailure
  | FailOnFailure
  deriving (Show)

data Renderer
  = PlantUML
  | Diagrams
  deriving (Show)

data EnumArrowsConfig
  = EnumArrowsConfig {
      sdConfig :: SDConfig
    , maxInstances :: Maybe Integer
    , syntaxWarnTooManyArrows :: Bool
    , printExtendedFeedback :: Bool
    , renamingStrategy :: RenamingStrategy
    , renderPath :: RenderPath
    , shuffle :: Maybe ShufflePolicy
  } deriving (Show)


defaultEnumArrowsConfig :: EnumArrowsConfig
defaultEnumArrowsConfig
  = EnumArrowsConfig {
      sdConfig = defaultSDConfig
    , maxInstances = Just 1000
    , printExtendedFeedback = False
    , syntaxWarnTooManyArrows = False
    , renamingStrategy = JustTheInnermostName
    , renderPath = RenderPath {
                     renderPolicy = RegenerateOnFailure
                   , renderer = PlantUML
                   }
    , shuffle = Nothing
  }

instance Randomise EnumArrowsInstance where
  randomise taskInstance@EnumArrowsInstance{ shufflePolicy
                                           , renamingPolicy
                                           , hierarchicalSD }
    = do
      hierarchicalSD'
        <- case shufflePolicy of
              DoNotShuffle
                -> return hierarchicalSD
              ShuffleNames
                -> shuffleNodeNames hierarchicalSD
              ShuffleTriggers
                -> shuffleTriggers' hierarchicalSD
              ShuffleNamesAndTriggers
                -> shuffleNodeNames hierarchicalSD >>= shuffleTriggers'
      let flatAndEnumeratedSD'
            = flattenAndEnumerate renamingPolicy hierarchicalSD'
      return
        taskInstance {
          hierarchicalSD
            = hierarchicalSD'
        , flatAndEnumeratedSD
            = flatAndEnumeratedSD'
        , taskSolution
            = correctEnumeration flatAndEnumeratedSD'
        }

shuffleNodeNames :: MonadRandom m => UMLStateDiagram String Int -> m (UMLStateDiagram String Int)
shuffleNodeNames hierarchicalSD
  = unUML (\_
            substates
            _
            _
             -> do
                let names'
                       = filter notNull $
                         head $ -- help
                         traverse (\case
                                     InnerMostState { name = innerName } -> [innerName]
                                     StateDiagram { name = "" } -> [""]  -- apparently, this is needed to avoid null returns
                                     StateDiagram { name = stateName } -> [stateName]
                                     _ -> [""]
                                  ) substates
                shuffledNames <- shuffleM names'
                let nameToShuffledName = zip names' shuffledNames
                let toShuffled name'
                        = fromMaybe name' (lookup name' nameToShuffledName)
                pure $ rename toShuffled hierarchicalSD
        ) hierarchicalSD

shuffleTriggers' :: (MonadRandom m) => UMLStateDiagram n a -> m (UMLStateDiagram n a)
shuffleTriggers' hierarchicalSD
  =
    unUML (\name
            substates
            connections
            startState
             -> do
                let withoutAndWithTriggers
                      = partition ((==) "" . transition) connections
                shuffledLiterals
                  <- shuffleM (map transition $ snd withoutAndWithTriggers)
                return $
                  umlStateDiagram $
                    StateDiagram {
                      name
                        = name
                    , substates
                        = substates
                    , connections
                        = fst withoutAndWithTriggers ++
                          zipWith (\c sl -> c {transition = sl})
                                  (snd withoutAndWithTriggers)
                                  shuffledLiterals
                    , startState
                        = startState
                    , label = error "THIS LABEL IS HIDDEN AND SHOULD NOT BE USED"
                    }) (globalise hierarchicalSD)

instance RandomiseLayout EnumArrowsInstance where
  randomiseLayout _
    = error "randomizing the chart layout would violate the constraints of the Alloy model"
      -- though we could shuffle the entries in all lists.
      -- that might alter the layout printed by the renderer
      -- while the chart remains semantically the same

enumArrows :: MonadIO m => EnumArrowsConfig -> Int -> m EnumArrowsInstance
enumArrows config timestamp
  = evalRandT (enumArrowsInstance config) (mkStdGen timestamp)

enumArrowsTask :: (OutputMonad m, MonadIO m) => FilePath -> EnumArrowsInstance -> LangM m
enumArrowsTask path task
  = do
    paragraph $ translate $ do
      english "Consider the following state chart."
    case chartRenderer task of
       PlantUML
         -> image $=<< liftIO $ drawSDToFile (combine path "plain") (hierarchicalSD task)
       Diagrams
         -> image $=<< do liftIO (renderSVG
                                 (combine path "plainDiagram.svg")
                                 (dims (V2 800 600))
                                 (drawDiagram Unstyled (hierarchicalSD task)))
                          return (combine path "plainDiagram.svg")
    paragraph $ translate $ do
      english "Which was flattened, having the transition literals replaced by integers."
    case chartRenderer task of
       PlantUML
         -> image $=<< liftIO $ drawSDToFile (combine path "flattened") (flatAndEnumeratedSD task)
       Diagrams
         -> image $=<< do liftIO (renderSVG
                                 (combine path "flattenedDiagram.svg")
                                 (dims (V2 800 600))
                                 (drawDiagram Unstyled (flatAndEnumeratedSD task)))
                          return (combine path "flattenedDiagram.svg")
    paragraph $ translate $ do
      english "Please supply a list of tuples, where the first element is the label of the transition as string\n\
               \ and the second element is the transition literal as string, that is supposed\
               \ to be at that place."
    paragraph $ translate $ do
      english "You may use the following syntax to denote the missing arrows:\n\
               \ [(\"1\",\"a\")] is a list, referring to a single transition labelled (1) that is supposed to be the literal 'a'."
    pure ()

enumArrowsInstance :: (RandomGen g, MonadIO m) => EnumArrowsConfig -> RandT g m EnumArrowsInstance
enumArrowsInstance EnumArrowsConfig { sdConfig
                                    , maxInstances = (Just maxInstances)
                                    , renamingStrategy
                                    , renderPath
                                    , shuffle
                                    }
  = do
    iterateUntil
      (\taskInstance
          -> case renderPath of
                RenderPath { renderPolicy = FailOnFailure
                           , renderer = PlantUML
                           }
                  -> (isNothing (checkDrawabilityPlantUML (hierarchicalSD taskInstance)) &&
                      isNothing (checkDrawabilityPlantUML (flatAndEnumeratedSD taskInstance)))
                      || error "PlantUML renderer failed"
                RenderPath { renderPolicy = RegenerateOnFailure
                           , renderer = PlantUML
                           }
                  -> isNothing (checkDrawabilityPlantUML (hierarchicalSD taskInstance)) &&
                     isNothing (checkDrawabilityPlantUML (flatAndEnumeratedSD taskInstance))
                RenderPath { renderPolicy = FailOnFailure
                            , renderer = Diagrams
                            }
                  -> isNothing (checkDrawability (hierarchicalSD taskInstance)) &&
                     isNothing (checkDrawability (flatAndEnumeratedSD taskInstance))
                     || error "Diagrams renderer failed"
                RenderPath { renderPolicy = RegenerateOnFailure
                           , renderer = Diagrams
                           }
                  -> isNothing (checkDrawability (hierarchicalSD taskInstance)) &&
                     isNothing (checkDrawability (flatAndEnumeratedSD taskInstance))
              )
      (do
       liftIO $ putStrLn "generating instance"
       inst <- liftIO $ getInstances (Just maxInstances) (sdConfigToAlloy sdConfig)
       r <- liftIO (randomRIO (0, fromIntegral maxInstances - 1) :: IO Int)
       liftIO $ putStrLn ("instance " ++ show r ++ " selected")
       let chart = map (failWith id . parseInstance "this") inst !! r
       let flatChart
             = flattenAndEnumerate renamingStrategy chart
       return EnumArrowsInstance {
           hierarchicalSD = chart
         , chartRenderer = renderer renderPath
         , taskSolution
             = correctEnumeration flatChart
         , flatAndEnumeratedSD
             = flatChart
         , shufflePolicy
             = fromMaybe DoNotShuffle shuffle
         , renamingPolicy = renamingStrategy
       }
      )
enumArrowsInstance _ = undefined

flattenAndEnumerate :: RenamingStrategy -> UMLStateDiagram String Int -> UMLStateDiagram String Int
flattenAndEnumerate renamingStrategy chart
  = umlStateDiagram $
               unUML (\name substates connection startState
                         -> StateDiagram {
                              name = name
                            , substates = substates
                            , connections
                                = zipWith (\c l
                                               -> c {transition = show l})
                                  connection ([1..]::[Int])
                            , startState = startState
                            , label = 999
                            }
                     ) (rename (case renamingStrategy of
                         HierarchicalConcatenation
                           -> concat
                         JustTheInnermostName
                           -> last
                      ) (flatten chart))

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

enumArrowsSyntax :: (OutputMonad m) => EnumArrowsInstance -> [(String,String)] -> LangM m
enumArrowsSyntax task answer
  = do
    assertion (any (\(i,_) -> 1 < length (filter ((==) i . fst) answer)) answer) $ translate $ do
      english "No enumeration was used more than once."
    --assertion (any (\(i,_) -> i < 1) answer) $ translate $ do
    --  english "Transition enumeration must be positive integers."
    assertion ( syntaxWarnTooManyArrows defaultEnumArrowsConfig &&
                length answer > (length (enumArrowsSolution task) +
               (length (enumArrowsSolution task) `div` 2))) $ translate $ do
      english "Transitions enumerated must not exceed the number of transitions in the chart."
    assertion (any (\(_,l) -> l == "") answer) $ translate $ do
      english "Transition literals must not be empty."
    assertion (null answer) $ translate $ do
      english "No empty list of tuples was supplied."
    return ()

enumArrowsEvaluation :: (OutputMonad m) => EnumArrowsInstance -> [(String,String)] -> Rated m
enumArrowsEvaluation task answer
  = printSolutionAndAssert
    (Just $ show (concatMap (uncurry zip) $ enumArrowsSolution task))
    (rate (enumArrowsSolution task) answer)

enumArrowsSolution :: EnumArrowsInstance -> [([String], [String])]
enumArrowsSolution EnumArrowsInstance {taskSolution}
  = taskSolution

correctEnumeration :: UMLStateDiagram String Int -> [([String], [String])]
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
                zip (map show ([1..]::[Int])) connection
    )

-- we must assert before calling this function that every label
-- is only used once in the submission; for all (i1,_) (i2,_) => i1 /= i2
rate :: [([String], [String])] -> [(String,String)] -> Rational
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

enumArrowsFeedback :: (OutputMonad m) => EnumArrowsInstance -> [(String,String)] -> LangM m
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
      = flattenAndEnumerate JustTheInnermostName flatCase1
  , taskSolution
      = correctEnumeration
        (rename last $ flatten flatCase1)
  , chartRenderer = PlantUML
  , shufflePolicy = DoNotShuffle
  , renamingPolicy = JustTheInnermostName
  }
