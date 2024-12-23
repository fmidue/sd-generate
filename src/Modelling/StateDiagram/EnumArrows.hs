
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Modelling.StateDiagram.EnumArrows (enumArrows
                                         ,enumArrowsTask
                                         ,enumArrowsInstance
                                         ,enumArrowsSyntax
                                         ,enumArrowsEvaluation
                                         ,EnumArrowsInstance(..)
                                         ,EnumArrowsConfig(..)
                                         ,defaultEnumArrowsConfig
                                         ,defaultEnumArrowsInstance
                                         ,rate
                                         ,enumArrowsFeedback
                                         ,checkEnumArrowsConfig
                                         ,checkEnumArrowsInstance
                                         ,randomise
                                         ,randomiseLayout
                                         ,ShufflePolicy(..))
where

{-
brief task description:

given a flattened state chart, where the transition triggers have been disguised through placeholder elements,
ask the user to supply a list of tuples, where the first element is the placeholder of the transition referenced as string
and the second element is the transition trigger as string that is supposed to be at that place.
When tasks instances are generated, the placeholder elements to disguise the triggers are an enumeration of these.
-}

import Modelling.StateDiagram.Datatype
    ( StateDiagram(..),
      UMLStateDiagram (unUML'),
      Connection(..),
      unUML,
      umlStateDiagram,
      collectNames,
      rename )
import Modelling.StateDiagram.Config(SDConfig(..)
                                    ,sdConfigToAlloy
                                    ,defaultSDConfig
                                    ,preventEmptyTriggersFromStates
                                    ,hierarchicalStates
                                    ,ChartLimits(..)
                                    ,checkSDConfig)
import Modelling.StateDiagram.Alloy()
import qualified Modelling.StateDiagram.PlantUMLDiagrams as PlantUML
  (drawSDToFile
  ,checkDrawabilityPlantUML)
import System.FilePath(combine)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  Rated,
  ($=<<),
  english,
  translate,
  printSolutionAndAssert, ArticleToUse (DefiniteArticle)
  )
import Control.Monad.Random
    ( MonadRandom,
      RandT,
      RandomGen,
      evalRandT,
      mkStdGen )
import Language.Alloy.Call (getInstances)
import Modelling.StateDiagram.Instance (parseInstance
                                       ,failWith)
import Modelling.StateDiagram.Flatten (flatten)
import Data.List (delete, sort)
import Control.Monad.Random.Lazy (randomRIO)
import Data.Either (rights)
import Control.Monad.Loops (iterateUntil)
import Modelling.StateDiagram.Checkers (checkDrawability)
import Data.Maybe (isNothing, fromMaybe, fromJust)
import Modelling.StateDiagram.Style (Styling(Unstyled))
import qualified Modelling.StateDiagram.Render as NativeRenderer (drawSDToFile)
import System.Random.Shuffle (shuffleM)

import Data.Time.Clock.POSIX(getPOSIXTime)
import Modelling.Auxiliary.Common
import Data.List.Extra (notNull
                       ,groupSortOn
                       ,nubOrd)
import Data.Bifunctor (bimap)
import Data.Ratio ((%))
import Data.Either.Extra (fromLeft', fromRight')

data EnumArrowsInstance
  = EnumArrowsInstance {
        hierarchicalSD :: UMLStateDiagram String Int
      , flatAndEnumeratedSD :: UMLStateDiagram [String] Int
      , taskSolution :: [([String], [String])]
      , chartRenderer :: Renderer
      , shuffle :: Maybe ShufflePolicy
      , renaming :: RenamingPolicy
      , randomization :: Bool
    } deriving (Show,Eq)

data ShufflePolicy
  = ShuffleNamesAndTriggers
  | ShuffleNames
  | ShuffleTriggers
  deriving (Show,Eq)

data RenamingPolicy
  = HierarchicalConcatenation
  | JustTheInnermostName
  deriving (Show,Eq)

data RenderPath
  = RenderPath {
      renderPolicy :: RenderPolicy
    , renderer :: Renderer
  } deriving (Show,Eq)

data RenderPolicy
  = RegenerateOnFailure
  | FailOnFailure
  deriving (Show,Eq)

data Renderer
  = PlantUML
  | Diagrams
  deriving (Eq, Show)

data EnumArrowsConfig
  = EnumArrowsConfig {
      sdConfig :: SDConfig
    , maxInstances :: Maybe Integer
    , syntaxWarnTooManyArrows :: Bool
    , printExtendedFeedback :: Bool
    , renamingPolicy :: RenamingPolicy
    , renderPath :: RenderPath
    , shufflePolicy :: Maybe ShufflePolicy
    , randomizeLayout :: Bool
  } deriving (Show,Eq)

defaultEnumArrowsConfig :: EnumArrowsConfig
defaultEnumArrowsConfig
  = EnumArrowsConfig {
      sdConfig = defaultSDConfig
    , maxInstances = Just 20
    , printExtendedFeedback = True
    , syntaxWarnTooManyArrows = True
    , renamingPolicy = HierarchicalConcatenation
    , renderPath = RenderPath {
                     renderPolicy = FailOnFailure
                   , renderer = Diagrams
                   }
    , shufflePolicy = Just ShuffleNamesAndTriggers
    , randomizeLayout = True
  }

instance Randomise EnumArrowsInstance where
  randomise taskInstance@EnumArrowsInstance{ shuffle }
    = do
      case shuffle of
        Nothing
          -> return taskInstance
        Just ShuffleNames
          -> shuffleNodeNames taskInstance
        Just ShuffleTriggers
          -> shuffleTriggers taskInstance
        Just ShuffleNamesAndTriggers
          -> shuffleNodeNames taskInstance >>= shuffleTriggers


shuffleNodeNames :: (MonadRandom m) => EnumArrowsInstance -> m EnumArrowsInstance
shuffleNodeNames task@EnumArrowsInstance {hierarchicalSD,flatAndEnumeratedSD}
  = do
    let names'
          = nubOrd $
            filter notNull $
            collectNames hierarchicalSD
            ++ concat (collectNames flatAndEnumeratedSD)
    shuffledNames <- shuffleM names'
    let toShuffled "" = ""
        toShuffled name'
          = fromJust (lookup name' (zip names' shuffledNames))
    return $
      task {
        hierarchicalSD
          = rename toShuffled hierarchicalSD
      , flatAndEnumeratedSD
          = rename (map toShuffled) flatAndEnumeratedSD
      }

shuffleTriggers :: (MonadRandom m) => EnumArrowsInstance -> m EnumArrowsInstance
shuffleTriggers task@EnumArrowsInstance {hierarchicalSD,flatAndEnumeratedSD,taskSolution}
  = do
    let uniqueTriggers
          = nubOrd . concatMap (map transition) . unUML' $ hierarchicalSD
    let placeholder
          = concatMap (map transition) . unUML' $ flatAndEnumeratedSD
    triggerToTrigger'
      <- zip uniqueTriggers <$> shuffleM uniqueTriggers
    placeholderToPlaceholder'
      <- zip placeholder <$> shuffleM placeholder
    return $
      task {
        hierarchicalSD
          = umlStateDiagram . fmap
            (shuffleTrigger triggerToTrigger') . unUML' $ hierarchicalSD
      , flatAndEnumeratedSD
          = umlStateDiagram . fmap
            (shuffleTrigger placeholderToPlaceholder') . unUML' $ flatAndEnumeratedSD
      , taskSolution
          = sort $
            map (bimap
                  (sort . map (replace placeholderToPlaceholder'))
                  (sort . map (replace triggerToTrigger')))
            taskSolution
      }
  where
    replace toShuffledTrigger trigger = fromMaybe
                          (error $
                          "trigger to shuffle not found " ++
                            show trigger ++ " in " ++ show toShuffledTrigger)
                          (lookup trigger toShuffledTrigger)
    shuffleTrigger toShuffledTrigger
      = map (\case
             c@Connection { transition = ""}
               -> c -- empty triggers are not shuffled
             c@Connection { transition = trigger }
               -> c { transition
                        = replace toShuffledTrigger trigger
                          -- triggers are shuffled uniformly
                    }
            )

shuffleSubstates :: (MonadRandom m) => UMLStateDiagram a b -> m (UMLStateDiagram a b)
shuffleSubstates
  = unUML (\name substates connections startState
             -> do
                substates' <- shuffleM =<< mapM recursivelyShuffleSubstates substates
                return $
                  umlStateDiagram $
                  StateDiagram {
                    name = name
                  , substates = substates'
                  , connections = connections
                  , startState = startState
                  , label = undefined
                  }
          )
  where
    -- recursivelyShuffleSubstates :: (MonadRandom m) => StateDiagram a b [Connection b] -> m (StateDiagram a b [Connection b])
    recursivelyShuffleSubstates StateDiagram { substates, .. }
      = do
        substates' <- shuffleM =<< mapM recursivelyShuffleSubstates substates
        return $
          StateDiagram {
            substates = substates'
          , ..
          }
    recursivelyShuffleSubstates CombineDiagram { substates, .. }
      = do
        substates' <- shuffleM =<< mapM recursivelyShuffleSubstates substates
        return $
          CombineDiagram {
              substates = substates'
            , ..
            }
    recursivelyShuffleSubstates x = return x

shuffleConnections :: (MonadRandom m) => UMLStateDiagram a b -> m (UMLStateDiagram a b)
shuffleConnections
  = fmap umlStateDiagram . traverse shuffleM . unUML'

instance RandomiseLayout EnumArrowsInstance where
  randomiseLayout taskInstance@EnumArrowsInstance{ hierarchicalSD, flatAndEnumeratedSD, randomization = True }
    = do
      hierarchicalSD'
        <- shuffleConnections =<< shuffleSubstates hierarchicalSD
      flatAndEnumeratedSD'
        <- shuffleConnections =<< shuffleSubstates flatAndEnumeratedSD
      return $
        taskInstance {
          hierarchicalSD
            = hierarchicalSD'
        , flatAndEnumeratedSD
            = flatAndEnumeratedSD'
      }
  randomiseLayout taskInstance = return taskInstance

enumArrows :: MonadIO m => EnumArrowsConfig -> Int -> m EnumArrowsInstance
enumArrows config timestamp
  = evalRandT (enumArrowsInstance config) (mkStdGen timestamp)

enumArrowsTask :: (OutputCapable m, MonadIO m) => FilePath -> EnumArrowsInstance -> LangM m
enumArrowsTask path task
  = do
    paragraph $ translate $ do
      english "Consider the following state chart."
    case chartRenderer task of
       PlantUML
         -> image $=<< liftIO $ PlantUML.drawSDToFile (combine path "plainDiagram.svg") (hierarchicalSD task)
       Diagrams
         -> image $=<< liftIO (NativeRenderer.drawSDToFile
                                 (combine path "plainDiagram.svg")
                                 (Just 800, Just 600)
                                 Unstyled
                                 (hierarchicalSD task))
    paragraph $ translate $ do
      english "Which was flattened, but has all transition triggers disguised through placeholder elements."
    let flatAndEnumeratedSD' -- renaming policy is just a view on data
          = case renaming task of
              HierarchicalConcatenation
                -> rename concat (flatAndEnumeratedSD task)
              JustTheInnermostName
                -> rename last (flatAndEnumeratedSD task)
    case chartRenderer task of
       PlantUML
         -> image $=<< liftIO $
                        PlantUML.drawSDToFile
                        (combine path "flattenedDiagram.svg")
                        flatAndEnumeratedSD'
       Diagrams
         -> image $=<< liftIO (NativeRenderer.drawSDToFile
                                 (combine path "flattenedDiagram.svg")
                                 (Just 800, Just 600)
                                 Unstyled
                                 flatAndEnumeratedSD')
    paragraph $ translate $ do
      english "Please supply a list of tuples, where the first element is the visible placeholder of a transition as string\n\
               \ and the second element is the transition trigger as string, that is supposed to be at that place."
    paragraph $ translate $ do
      english "You may use the following syntax:\n\
               \ [(\"1\",\"a\")] is a list, referring to a single transition labelled (1) that is supposed to be triggered by 'a'."
    pure ()

enumArrowsInstance :: (RandomGen g, MonadIO m) => EnumArrowsConfig -> RandT g m EnumArrowsInstance
enumArrowsInstance EnumArrowsConfig { sdConfig
                                    , maxInstances = (Just maxInstances)
                                    , renamingPolicy
                                    , renderPath
                                    , shufflePolicy
                                    , randomizeLayout
                                    }
  = do
    iterateUntil
      (\EnumArrowsInstance{hierarchicalSD, flatAndEnumeratedSD}
        -> canBothBeDrawnVia renderPath hierarchicalSD renamingPolicy flatAndEnumeratedSD
      )
      (do
       liftIO $ putStrLn "generating instance"
       start <- liftIO getPOSIXTime
       inst <- liftIO $ getInstances (Just maxInstances) (sdConfigToAlloy sdConfig)
       r <- liftIO (randomRIO (0, fromIntegral maxInstances - 1) :: IO Int)
       liftIO $ putStrLn ("instance " ++ show r ++ " selected of " ++ show (length inst) ++ " instances")
       let chart = map (failWith show . parseInstance "this") inst !! r
       stop <- liftIO getPOSIXTime
       liftIO $ putStrLn ("instance generation took " ++ show (stop - start) ++ " seconds")
       return $
        flip unUML (flatten chart) $
        \name substates connections startState ->
         let placeholdersWithNonEmptyConnections =
               zip (map show ([1..] :: [Int])) $ filter (notNull . transition) connections
         in
         EnumArrowsInstance {
           hierarchicalSD = chart
         , chartRenderer = renderer renderPath
         , taskSolution = sort . map (bimap sort (sort . map transition) . unzip)
                           $
                           groupSortOn (\(_,x)
                                             -> (pointFrom x, pointTo x))
                           placeholdersWithNonEmptyConnections
         , flatAndEnumeratedSD =
                            umlStateDiagram $
                            StateDiagram {
                              name = name
                            , substates = substates
                            , connections
                                = filter (null . transition) connections
                                  ++
                                  map (\(placeholder,c)
                                         -> c { transition = placeholder })
                                  placeholdersWithNonEmptyConnections
                            , startState = startState
                            , label = undefined
                            }
         , shuffle
             = shufflePolicy
         , renaming
             = renamingPolicy
         , randomization
             = randomizeLayout
         }
      )
enumArrowsInstance _ = undefined

canBothBeDrawnVia :: RenderPath -> UMLStateDiagram String Int -> RenamingPolicy -> UMLStateDiagram [String] Int -> Bool
canBothBeDrawnVia renderPath hierarchicalSD renaming flatAndEnumeratedSD =
  case renderPath of
    RenderPath { renderPolicy = FailOnFailure
               , renderer = PlantUML
               }
      -> workingPlantUML || error "PlantUML renderer failed"
    RenderPath { renderPolicy = RegenerateOnFailure
               , renderer = PlantUML
               }
      -> workingPlantUML
    RenderPath { renderPolicy = FailOnFailure
               , renderer = Diagrams
               }
      -> workingDiagrams || error "Diagrams renderer failed"
    RenderPath { renderPolicy = RegenerateOnFailure
               , renderer = Diagrams
               }
      -> workingDiagrams
  where
    flatAndEnumeratedSD'
      = case renaming of
          HierarchicalConcatenation
            -> rename concat flatAndEnumeratedSD
          JustTheInnermostName
            -> rename last flatAndEnumeratedSD
    workingPlantUML =
      isNothing (PlantUML.checkDrawabilityPlantUML hierarchicalSD) &&
      isNothing (PlantUML.checkDrawabilityPlantUML flatAndEnumeratedSD')
    workingDiagrams =
      isNothing (checkDrawability hierarchicalSD) &&
      isNothing (checkDrawability flatAndEnumeratedSD')

checkEnumArrowsInstance :: EnumArrowsInstance -> Maybe String
checkEnumArrowsInstance EnumArrowsInstance{..}
  | chartRenderer /= Diagrams && randomization
    = Just "Chart layout randomization is not supported for other renderers than Diagrams when enabled."
  | not $ canBothBeDrawnVia RenderPath{renderer = chartRenderer, renderPolicy = RegenerateOnFailure} hierarchicalSD renaming flatAndEnumeratedSD
    = Just "The rendering settings lead to failure."
  | otherwise = Nothing

checkEnumArrowsConfig :: EnumArrowsConfig -> Maybe String
checkEnumArrowsConfig EnumArrowsConfig{ sdConfig
                                          = sdConfig@SDConfig { chartLimits = ChartLimits { .. }
                                                              , .. }
                                      , randomizeLayout
                                      , renderPath }
  | fst startNodes < 1
  = Just "Tasks predicated on reachability will have at least a start node on the outermost level."
  | hierarchicalStates < 1
  = Just "The chart must have at least one hierarchical state."
  | not preventTriggerNamesDuplication
  = Just "For this task type, triggers in the original diagram should be all made distinct."
  | renderer renderPath /= Diagrams && randomizeLayout
  = Just "Chart layout randomization is not supported for other renderers than Diagrams when enabled."
  | not preventNormalStateNamesDuplication
  = Just "The chart should have distinct normal state names to be uniquely solvable."
  | regions > 0
  = Just "Flattening does not support regions currently."
  | snd shallowHistoryNodes > 0 || snd deepHistoryNodes > 0
  = Just "Flattening does not support history nodes."
  | otherwise = checkSDConfig sdConfig

enumArrowsSyntax :: (OutputCapable m) => EnumArrowsInstance -> [(String,String)] -> LangM m
enumArrowsSyntax task answer
  = do
    assertion (length (nubOrd $ map fst answer) == length answer) $ translate $ do
      english "No placeholder was used more than once."
    assertion (not ( syntaxWarnTooManyArrows defaultEnumArrowsConfig &&
                length answer > sum (map (length . fst) (taskSolution task)))) $ translate $ do
      english "The number of triggers matched against placeholder elements for transitions must not exceed the number of transitions in the chart."
    assertion (not (any (\(_,l) -> l == "") answer)) $ translate $ do
      english "Transition triggers must not be empty."
    assertion (not (null answer)) $ translate $ do
      english "No empty list of tuples was supplied."
    return ()

enumArrowsEvaluation :: (OutputCapable m) => EnumArrowsInstance -> [(String,String)] -> Rated m
enumArrowsEvaluation task answer
  = printSolutionAndAssert
    DefiniteArticle
    (Just $ show (concatMap (uncurry zip) $ taskSolution task))
    (rate (taskSolution task) answer)

missing :: (Eq a, Eq b) => [([a], [b])] -> [(a, b)] -> [(a, b)]
missing solution submission
      = concatMap (uncurry zip) $
        filter (\case
                  ([],[]) -> False
                  _ -> True) $
        foldl (\solution' (p,t)
                 -> map (\(ps,ts)
                           -> if p `elem` ps && t `elem` ts
                              then (,) (delete p ps) (delete t ts)
                              else (ps,ts)
                        ) solution'
              ) solution submission


rated :: (Eq b, Eq d) => [([b], [d])] -> [(b, d)] -> [Either (b, d) (b, d)]
rated solution submission
      = map (\case
               r@(Right _,Right _) -> Right (bimap fromRight' fromRight' r)
               l@(Left _,Left _) -> Left (bimap fromLeft' fromLeft' l)
               _ -> error "rated: impossible")
        .
        concatMap (uncurry zip) $
        foldl (\solution' (p,t)
                 -> map (\(ps,ts)
                           -> if p `elem` ps && t `elem` ts
                              then (,) (Right (fromLeft' p):delete p ps)
                                       (Right (fromLeft' t):delete t ts)
                              else (ps,ts)
                        ) solution'
              )
         (map (bimap (map Left) (map Left)) solution)
         (map (bimap Left Left ) submission)


rate :: [([String], [String])] -> [(String,String)] -> Rational
rate solution submission
  = fromIntegral (length . rights $ rated solution submission)
    %
    fromIntegral (sum $ map (length . snd) solution)

enumArrowsFeedback :: (OutputCapable m) => EnumArrowsInstance -> [(String,String)] -> LangM m
enumArrowsFeedback task submission
  = let
    solution = taskSolution task
    rated' = rights $ rated solution submission
    in
    do
    paragraph $ translate $ do
      english ("correct: " ++ show rated' ++ "\n" ++
               "wrong: " ++ show (filter (flip notElem (map fst rated') . fst) submission) ++ "\n" ++
               "missing: " ++ show (missing solution submission) ++ "\n")
    return ()

defaultEnumArrowsInstance :: EnumArrowsInstance
defaultEnumArrowsInstance
  = EnumArrowsInstance {
    hierarchicalSD
      = umlStateDiagram $
        StateDiagram { substates = [ StateDiagram { substates = [ InnerMostState {label = 1, name = "G", operations = ""}
                                                                , InnerMostState {label = 2, name = "H", operations = ""}
                                                                , InnerMostState {label = 3, name = "C", operations = ""}
                                                                , InnerMostState {label = 4, name = "B", operations = ""}]
                                                  , label = 1, name = ""
                                                  , connections = [ Connection {pointFrom = [1], pointTo = [2], transition = "f"}
                                                                  , Connection {pointFrom = [3], pointTo = [1], transition = "c"}]
                                                  , startState = []}
                                   , InnerMostState {label = 2, name = "D", operations = ""}
                                   , InnerMostState {label = 3, name = "F", operations = ""}
                                   , InnerMostState {label = 4, name = "E", operations = ""}
                                   , InnerMostState {label = 5, name = "A", operations = ""}]
                      , label = undefined
                      , name = ""
                      , connections = [ Connection {pointFrom = [1], pointTo = [5], transition = "g"}
                                      , Connection {pointFrom = [1,2], pointTo = [5], transition = "b"}
                                      , Connection {pointFrom = [1,4], pointTo = [5], transition = "d"}
                                      , Connection {pointFrom = [2], pointTo = [3], transition = "h"}
                                      , Connection {pointFrom = [3], pointTo = [1,3], transition = "i"}
                                      , Connection {pointFrom = [4], pointTo = [2], transition = "e"}
                                      , Connection {pointFrom = [5], pointTo = [1,4], transition = "a"} ]
                      , startState = [4] }
  , flatAndEnumeratedSD
      = umlStateDiagram $
        StateDiagram { substates = [ InnerMostState {label = 1, name = ["","G"], operations = ""}
                                   , InnerMostState {label = 2, name = ["","H"], operations = ""}
                                   , InnerMostState {label = 3, name = ["","C"], operations = ""}
                                   , InnerMostState {label = 4, name = ["","B"], operations = ""}
                                   , InnerMostState {label = 5, name = ["D"], operations = ""}
                                   , InnerMostState {label = 6, name = ["F"], operations = ""}
                                   , InnerMostState {label = 7, name = ["E"], operations = ""}
                                   , InnerMostState {label = 8, name = ["A"], operations = ""}]
                    , label = undefined
                    , name = [""]
                    , connections = [ Connection {pointFrom = [1], pointTo = [8], transition = "8"}
                                    , Connection {pointFrom = [2], pointTo = [8], transition = "11"}
                                    , Connection {pointFrom = [3], pointTo = [8], transition = "4"}
                                    , Connection {pointFrom = [4], pointTo = [8], transition = "6"}
                                    , Connection {pointFrom = [2], pointTo = [8], transition = "3"}
                                    , Connection {pointFrom = [4], pointTo = [8], transition = "10"}
                                    , Connection {pointFrom = [5], pointTo = [6], transition = "12"}
                                    , Connection {pointFrom = [6], pointTo = [3], transition = "5"}
                                    , Connection {pointFrom = [7], pointTo = [5], transition = "9"}
                                    , Connection {pointFrom = [8], pointTo = [4], transition = "2"}
                                    , Connection {pointFrom = [1], pointTo = [2], transition = "1"}
                                    , Connection {pointFrom = [3], pointTo = [1], transition = "7"}]
                    , startState = [7]}
  , taskSolution
      = [(["1"],["f"]),(["8"],["g"]),(["11","3"],["g","b"]),(["7"],["c"])
        ,(["4"],["g"]),(["6","10"],["g","d"]),(["12"],["h"]),(["5"],["i"])
        ,(["9"],["e"]),(["2"],["a"])]
  , chartRenderer = PlantUML
  , shuffle = Just ShuffleNamesAndTriggers
  , renaming = JustTheInnermostName
  , randomization = False
  }
