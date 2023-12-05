
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
                                         ,randomiseLayout
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
                                    ,ChartLimits(..))
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
    ( MonadRandom (getRandom),
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
                 --, delete
                 )
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
import System.Random.Shuffle (shuffleM, shuffle')

import Data.Time.Clock.POSIX(getPOSIXTime)
import Modelling.Auxiliary.Common
import Data.List.Extra (notNull, nubOrd
--, nubOrdBy
  )
data EnumArrowsInstance
  = EnumArrowsInstance {
        hierarchicalSD :: UMLStateDiagram String Int
      , flatAndEnumeratedSD :: UMLStateDiagram [String] Int
      , taskSolution :: [([String], [String])]
      , chartRenderer :: Renderer
      , shuffle :: Maybe ShufflePolicy
      , renaming :: RenamingPolicy
    } deriving Show

data ShufflePolicy
  = ShuffleNamesAndTriggers
  | ShuffleNames
  | ShuffleTriggers
  deriving Show

data RenamingPolicy
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
    , renamingPolicy :: RenamingPolicy
    , renderPath :: RenderPath
    , shufflePolicy :: Maybe ShufflePolicy
  } deriving (Show)

defaultEnumArrowsConfig :: EnumArrowsConfig
defaultEnumArrowsConfig
  = EnumArrowsConfig {
      sdConfig = defaultSDConfig
    , maxInstances = Just 1000
    , printExtendedFeedback = False
    , syntaxWarnTooManyArrows = False
    , renamingPolicy = JustTheInnermostName
    , renderPath = RenderPath {
                     renderPolicy = RegenerateOnFailure
                   , renderer = PlantUML
                   }
    , shufflePolicy = Just ShuffleNamesAndTriggers
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
          = filter notNull $
            collectNames hierarchicalSD
    shuffledNames <- shuffleM names'
    let nameToShuffledName = zip names' shuffledNames
    let toShuffled name'
          = fromMaybe name' (lookup name' nameToShuffledName)
    let toShuffled' = map (\n -> fromMaybe n (lookup n nameToShuffledName))
    return $
      task {
        hierarchicalSD
          = rename toShuffled hierarchicalSD
      , flatAndEnumeratedSD
          = rename toShuffled' flatAndEnumeratedSD
      }

shuffleTriggers :: (MonadRandom m) => EnumArrowsInstance -> m EnumArrowsInstance
shuffleTriggers task@EnumArrowsInstance {hierarchicalSD,flatAndEnumeratedSD,taskSolution}
  = do
    let allTriggers
          = map snd . filter ((/=) "" . snd) $ concatMap (uncurry zip) taskSolution
    let uniqueTriggers
          = nubOrd allTriggers
    let placeholders
          = nubOrd . map fst . filter ((/=) "" . snd) $ concatMap (uncurry zip) taskSolution
    triggerToTrigger'
      <- shuffleM uniqueTriggers
         >>= \shuffledUnique -> return $ zip uniqueTriggers shuffledUnique
    let placeholderToPlaceholder'
          = [(p,p') |
               (p,t) <- zip placeholders allTriggers
               , (trigger,trigger') <- triggerToTrigger'
               , t == trigger
               , (t',p') <- zip allTriggers placeholders
               , trigger' == t']
    -- this code will not work because its not aware of trigger unfold size
    --let matchToPlaceholder triggerToPlaceholder (c:connections)
    --      | transition c /= ""
    --      = case find ((==) (transition c) . fst) (zip placeholders allTriggers) of
    --          Just (_,t)
    --            -> case find ((==) t . fst) triggerToTrigger' of
    --                 Just (_,t')
    --                   -> case find ((==) t' . fst) triggerToPlaceholder of
    --                        Just match@(_,p')
    --                          -> (:)
    --                             c { transition = p' } $
    --                             matchToPlaceholder (delete match triggerToPlaceholder) connections
    --                        _ -> error $ "cant find trigger in triggerToPlaceholder " ++ t' ++ " " ++ show triggerToPlaceholder
    --                 _ -> error $ "cant find trigger in triggerToTrigger' " ++ t ++ " " ++ show triggerToTrigger'
    --          Nothing -> error $ "cant find transition in placeholders " ++ show (transition c) ++ " " ++ show placeholders
    --     | transition c == ""
    --     = error "empty transition" -- c : matchToPlaceholder triggerToPlaceholder connections
    --    matchToPlaceholder [] [] = []
    --    matchToPlaceholder _ _ = error "asymetrical reduction"
    return $
      task {
        hierarchicalSD
          = umlStateDiagram . fmap
            (shuffleTrigger triggerToTrigger') . unUML' $ hierarchicalSD
      , flatAndEnumeratedSD
        -- = umlStateDiagram . fmap (matchToPlaceholder (zip allTriggers placeholders)) . unUML' $ flatAndEnumeratedSD
         = umlStateDiagram . fmap
           (shuffleTrigger placeholderToPlaceholder') . unUML' $ flatAndEnumeratedSD
      , taskSolution
          = map (unzip .
                 map (\(placeholder,trigger)
                         -> case trigger of
                              "" -> (,) placeholder trigger
                              _ -> (,)
                                   (fromMaybe
                                   (error $ "no placeholder found for " ++
                                      placeholder ++ " in " ++ show placeholderToPlaceholder')
                                   (lookup placeholder placeholderToPlaceholder'))
                                   (fromMaybe
                                   (error $ "no trigger found for " ++
                                      trigger ++ " in " ++ show triggerToTrigger')
                                   (lookup trigger triggerToTrigger'))
                     )
                . uncurry zip)
            taskSolution
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

overSubstates :: ([StateDiagram a Int [Connection Int]] -> [StateDiagram a Int [Connection Int]]) -> UMLStateDiagram a Int -> UMLStateDiagram a Int
overSubstates g
  = unUML (\name substates connections startState
             -> umlStateDiagram $
                StateDiagram {
                  name = name
                , substates = g (map (recurseSubstates g) substates)
                , connections = connections
                , startState = startState
                , label = error "THIS LABEL IS HIDDEN AND SHOULD NOT BE USED"
                }
          )
  where
    recurseSubstates f StateDiagram { substates
                                    , .. }
      = StateDiagram {
          substates = f (map (recurseSubstates f) substates)
        , ..
        }
    recurseSubstates f CombineDiagram { substates
                                      , .. }
      = CombineDiagram {
          substates = f (map (recurseSubstates f) substates)
        , ..
        }
    recurseSubstates _ x = x

instance RandomiseLayout EnumArrowsInstance where
  randomiseLayout taskInstance@EnumArrowsInstance{ hierarchicalSD, flatAndEnumeratedSD, chartRenderer }
    = do
      case chartRenderer of
        PlantUML
          -> error "PlantUML is not impacted by this type of layout randomisation, aborting."
        Diagrams
          -> do
             r <- getRandom
             let gen = mkStdGen r
             return $
               taskInstance {
                 hierarchicalSD
                   = umlStateDiagram . fmap (\c -> shuffle' c (length c) gen) . unUML'  $
                     overSubstates (\s -> shuffle' s (length s) gen) hierarchicalSD
               , flatAndEnumeratedSD
                   = umlStateDiagram . fmap (\c -> shuffle' c (length c) gen) . unUML' $
                     overSubstates (\s -> shuffle' s (length s) gen) flatAndEnumeratedSD
               }

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
      english "Which was flattened, but has all transition triggers disguised through placeholders."
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
             in
             case renderPath of
                RenderPath { renderPolicy = FailOnFailure
                           , renderer = PlantUML
                           }
                  -> (isNothing (checkDrawabilityPlantUML (hierarchicalSD taskInstance)) &&
                      isNothing (checkDrawabilityPlantUML flatAndEnumeratedSD'))
                      || error "PlantUML renderer failed"
                RenderPath { renderPolicy = RegenerateOnFailure
                           , renderer = PlantUML
                           }
                  -> isNothing (checkDrawabilityPlantUML (hierarchicalSD taskInstance)) &&
                     isNothing (checkDrawabilityPlantUML flatAndEnumeratedSD')
                RenderPath { renderPolicy = FailOnFailure
                            , renderer = Diagrams
                            }
                  -> isNothing (checkDrawability (hierarchicalSD taskInstance)) &&
                     isNothing (checkDrawability flatAndEnumeratedSD')
                     || error "Diagrams renderer failed"
                RenderPath { renderPolicy = RegenerateOnFailure
                           , renderer = Diagrams
                           }
                  -> isNothing (checkDrawability (hierarchicalSD taskInstance)) &&
                     isNothing (checkDrawability flatAndEnumeratedSD')
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
             = correctEnumeration flattenedChart
         , flatAndEnumeratedSD
             = enumerateTriggers flattenedChart
         , shuffle
             = shufflePolicy
         , renaming = renamingPolicy
       }
      )
enumArrowsInstance _ = undefined

enumerateTriggers :: UMLStateDiagram [String] Int -> UMLStateDiagram [String] Int
enumerateTriggers chart
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
                     ) chart

enumArrowsInstanceCheck :: (MonadIO m, MonadRandom m) => EnumArrowsConfig -> EnumArrowsInstance -> m (Maybe String)
enumArrowsInstanceCheck _ task
  | length (enumArrowsSolution task) > 30
    = return $ Just "The solution chart exceeds a reasonable amount of transitions, it would be tedious to enumerate them all."
  | otherwise = return Nothing

checkEnumArrowsConfig :: EnumArrowsConfig -> Maybe String
checkEnumArrowsConfig EnumArrowsConfig{ sdConfig = SDConfig { chartLimits = ChartLimits { .. }, .. } }
  | not preventEmptyTriggersFromStates
  = Just "The chart may contain empty triggers from states, which are not allowed in this task setting."
  | hierarchicalStates < 1
  = Just "The chart must have at least one hierarchical state."
  | not distinctTriggerNames
  = Just "For this task type, triggers in the original diagram should be all made distinct."
  | otherwise = Nothing

enumArrowsSyntax :: (OutputMonad m) => EnumArrowsInstance -> [(String,String)] -> LangM m
enumArrowsSyntax task answer
  = do
    assertion (not (any (\(i,_) -> 1 < length (filter ((==) i . fst) answer)) answer)) $ translate $ do
      english ("No placeholder was used more than once. \n" ++ show answer)
    assertion (not ( syntaxWarnTooManyArrows defaultEnumArrowsConfig &&
                length answer > length (enumArrowsSolution task))) $ translate $ do
      english "The number of triggers matched to their placeholders must not exceed the number of transitions in the chart."
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

correctEnumeration :: UMLStateDiagram [String] Int -> [([String], [String])]
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

-- newtype Trigger = Trigger String deriving (Show, Eq, Ord)
-- newtype Placeholder = Placeholder String deriving (Show, Eq, Ord)


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
    hierarchicalSD = flatCase1
  , flatAndEnumeratedSD
      = enumerateTriggers (flatten flatCase1)
  , taskSolution
      = correctEnumeration
        (flatten flatCase1)
  , chartRenderer = PlantUML
  , shuffle = Nothing
  , renaming = JustTheInnermostName
  }
