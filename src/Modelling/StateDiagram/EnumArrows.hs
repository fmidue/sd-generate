
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}


module Modelling.StateDiagram.EnumArrows (enumArrows
                                         ,enumArrowsTask
                                         ,enumArrowsSolution
                                         ,enumArrowsInstance
                                         ,enumArrowsSyntax
                                         ,enumArrowsEvaluation
                                         ,EnumArrowsInstance(..)
                                         ,EnumArrowsConfig(..)
                                         ,defaultEnumArrowsConfig
                                         ,defaultEnumInstance
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

given a flattened state chart, where the transition literals have been disguised through by placeholder elements,
ask the user to supply a list of tuples, where the first element is the placeholder of the transition referenced as string
and the second element is the transition trigger as string that is supposed to be at that place,
for all enumerated arrows of the chart.
when tasks instances are generated, the placeholder elements to disguise the triggers are an enumeration of these.
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
import Data.List (groupBy
                 ,singleton
                 ,sortBy
                 ,find
                 )
import Control.Monad.Random.Lazy (randomRIO)
import Data.Either (rights
                   ,lefts)
import Control.Monad.Loops (iterateUntil)
import Modelling.StateDiagram.Checkers (checkDrawability)
import Data.Maybe (isNothing, fromMaybe, fromJust)
import Modelling.StateDiagram.Layout (drawDiagram)
import Modelling.StateDiagram.Style (Styling(Unstyled))
import Diagrams.Backend.SVG (renderSVG)
import Diagrams (dims, V2 (V2))
import System.Random.Shuffle (shuffleM)

import Data.Time.Clock.POSIX(getPOSIXTime)
import Modelling.Auxiliary.Common
import Data.List.Extra (notNull
                       ,nubOrd)


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
    , maxInstances = Just 1000
    , printExtendedFeedback = False
    , syntaxWarnTooManyArrows = False
    , renamingPolicy = JustTheInnermostName
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
    let allTriggers
          = map snd . filter ((/=) "" . snd) $ concatMap (uncurry zip) taskSolution
    let uniqueTriggers
          = nubOrd allTriggers
    let placeholder
          = nubOrd . map fst . filter ((/=) "" . snd) $ concatMap (uncurry zip) taskSolution
    triggerToTrigger'
      <- shuffleM uniqueTriggers
         >>= \shuffledUnique -> return $ zip uniqueTriggers shuffledUnique
    placeholderToPlaceholder'
      <- shuffleM placeholder
         >>= \shuffledPlaceholder -> return $ zip placeholder shuffledPlaceholder
    return $
      task {
        hierarchicalSD
          = umlStateDiagram . fmap
            (shuffleTrigger triggerToTrigger') . unUML' $ hierarchicalSD
      , flatAndEnumeratedSD
          = umlStateDiagram . fmap
            (shuffleTrigger placeholderToPlaceholder') . unUML' $ flatAndEnumeratedSD
      , taskSolution
          = let
            match group
              = [(p', t') |
                   (placeholder'', trigger) <- group,
                   (p, p') <- placeholderToPlaceholder',
                   placeholder'' == p,
                   (t, t') <- triggerToTrigger',
                   trigger == t]
            in
            map (unzip . match . uncurry zip) taskSolution
      }
  where
    shuffleTrigger toShuffledTrigger
      = map (\case
             c@Connection { transition = ""}
               -> c -- empty triggers are not shuffled
             c@Connection { transition = trigger }
               -> c { transition
                        = fromMaybe
                          (error $
                          "trigger to shuffle not found" ++
                            show trigger ++ "in " ++ show toShuffledTrigger)
                          (lookup trigger toShuffledTrigger)
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
                  , label = error "THIS LABEL IS HIDDEN AND SHOULD NOT BE USED"
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

shuffleConnections :: (MonadRandom m) => UMLStateDiagram a Int -> m (UMLStateDiagram a Int)
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
                        drawSDToFile
                        (combine path "flattened")
                        flatAndEnumeratedSD'
       Diagrams
         -> image $=<< do liftIO (renderSVG
                                 (combine path "flattenedDiagram.svg")
                                 (dims (V2 800 600))
                                 (drawDiagram Unstyled
                                  flatAndEnumeratedSD'))
                          return (combine path "flattenedDiagram.svg")
    paragraph $ translate $ do
      english "Please supply a list of tuples, where the first element is the visible placeholder of a transition as string\n\
               \ and the second element is the transition trigger as string, that is supposed to be at that place."
    paragraph $ translate $ do
      english "You may use the following syntax to denote the missing arrows:\n\
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
      (\taskInstance
          -> let flatAndEnumeratedSD'
                   = case renamingPolicy of
                       HierarchicalConcatenation
                         -> rename concat (flatAndEnumeratedSD taskInstance)
                       JustTheInnermostName
                         -> rename last (flatAndEnumeratedSD taskInstance)
                 workingPlantUML =
                   isNothing (checkDrawabilityPlantUML (hierarchicalSD taskInstance)) &&
                   isNothing (checkDrawabilityPlantUML flatAndEnumeratedSD')
                 workingDiagrams =
                   isNothing (checkDrawability (hierarchicalSD taskInstance)) &&
                   isNothing (checkDrawability flatAndEnumeratedSD')
             in
             case renderPath of
                RenderPath { renderPolicy = FailOnFailure
                           , renderer = PlantUML
                           }
                  -> workingPlantUML
                      || error "PlantUML renderer failed"
                RenderPath { renderPolicy = RegenerateOnFailure
                           , renderer = PlantUML
                           }
                  -> workingPlantUML
                RenderPath { renderPolicy = FailOnFailure
                            , renderer = Diagrams
                            }
                  -> workingDiagrams
                     || error "Diagrams renderer failed"
                RenderPath { renderPolicy = RegenerateOnFailure
                           , renderer = Diagrams
                           }
                  -> workingDiagrams
              )
      (do
       liftIO $ putStrLn "generating instance"
       start <- liftIO getPOSIXTime
       inst <- liftIO $ getInstances (Just maxInstances) (sdConfigToAlloy sdConfig)
       r <- liftIO (randomRIO (0, fromIntegral maxInstances - 1) :: IO Int)
       liftIO $ putStrLn ("instance " ++ show r ++ " selected of " ++ show (length inst) ++ " instances")
       let chart = map (failWith id . parseInstance "this") inst !! r
       stop <- liftIO getPOSIXTime
       liftIO $ putStrLn ("instance generation took " ++ show (stop - start) ++ " seconds")
       let flattenedChart = flatten chart
       return EnumArrowsInstance {
           hierarchicalSD = chart
         , chartRenderer = renderer renderPath
         , taskSolution
             = unUML (\_ _ connections _
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
                           placeholderTo connections
                    ) flattenedChart
         , flatAndEnumeratedSD
             = umlStateDiagram $
               unUML (\name substates connections startState
                         -> StateDiagram {
                              name = name
                            , substates = substates
                            , connections
                                = filter (null . transition) connections
                                  ++
                                  map (\(placeholder,c)
                                         -> c { transition = placeholder })
                                  (placeholderTo connections)
                            , startState = startState
                            , label = 999
                            }
                     ) flattenedChart
         , shuffle
             = shufflePolicy
         , renaming
             = renamingPolicy
         , randomization
             = randomizeLayout
       }
      )
  where
    placeholderTo connection
      = zip (map show ([1..]::[Int]))
          $ filter (not . null . transition) connection
enumArrowsInstance _ = undefined

checkEnumArrowsInstance :: (MonadIO m, MonadRandom m) => EnumArrowsConfig -> EnumArrowsInstance -> m (Maybe String)
checkEnumArrowsInstance _ task
  | length (enumArrowsSolution task) > 30
    = return $ Just "The solution chart exceeds a reasonable amount of transitions, it would be tedious to enumerate them all."
  | otherwise = return Nothing

checkEnumArrowsConfig :: EnumArrowsConfig -> Maybe String
checkEnumArrowsConfig EnumArrowsConfig{ sdConfig
                                          = sdConfig@SDConfig { chartLimits = ChartLimits { .. }
                                                              , .. }
                                      , randomizeLayout
                                      , renderPath }
  | not preventEmptyTriggersFromStates
  = Just "The chart may contain empty triggers from states, which are not allowed in this task setting."
  | hierarchicalStates < 1
  = Just "The chart must have at least one hierarchical state."
  | not distinctTriggerNames
  = Just "For this task type, triggers in the original diagram should be all made distinct."
  | renderer renderPath /= Diagrams && randomizeLayout
  = Just "Chart layout randomization is not supported for other renderers than Diagrams when enabled."
  | not enforceNormalStateNames && not distinctNormalStateNames
  = Just "The chart should have distinct normal state names."
  | otherwise = checkSDConfig sdConfig

enumArrowsSyntax :: (OutputMonad m) => EnumArrowsInstance -> [(String,String)] -> LangM m
enumArrowsSyntax task answer
  = do
    assertion (not (any (\(i,_) -> 1 < length (filter ((==) i . fst) answer)) answer)) $ translate $ do
      english ("No placeholder was used more than once. \n" ++ show answer)
    assertion (not ( syntaxWarnTooManyArrows defaultEnumArrowsConfig &&
                length answer > length (enumArrowsSolution task))) $ translate $ do
      english "The number of triggers matched against placeholder elements for transitions must not exceed the number of transitions in the chart."
    assertion (not (any (\(_,l) -> l == "") answer)) $ translate $ do
      english "Transition triggers must not be empty."
    assertion (not (null answer)) $ translate $ do
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
           ) (nubOrd submission)
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
           ) (nubOrd submission)
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
                      , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
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
                    , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
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
