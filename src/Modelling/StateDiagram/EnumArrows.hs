
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Modelling.StateDiagram.EnumArrows (enumArrowsTask
                                         ,enumArrowsSolution
                                         ,enumArrowsInstance
                                         ,enumArrowsSyntax
                                         ,enumArrowsEvaluation
                                         ,EnumArrowsInstance(..)
                                         ,EnumArrowsConfig(..)
                                         ,defaultEnumArrowsConfig
                                         ,defaultEnumInstance
                                         ,enumArrowsFeedback
                                         ,enumArrows
                                         ,enumArrowsInstanceCheck)
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
      Connection(transition),
      unUML,
      umlStateDiagram,
      rename )
import Modelling.StateDiagram.Config(SDConfig
                                    ,sdConfigToAlloy
                                    ,defaultSDConfig)
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
  german,
  translate,
  printSolutionAndAssert, Language, GenericLangM (..)
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
import Modelling.StateDiagram.Flatten (flatten,mergeEqSourceAndTarget)
import Data.Maybe(isNothing)

import Data.List(groupBy,nub,sort, intercalate)
import Data.Tuple.Extra(second)

import Modelling.StateDiagram.Example (flatCase1)
import Control.Monad.Random.Lazy (randomRIO)

newtype EnumArrowsInstance
  = EnumArrowsInstance {
      hierarchicalSD :: UMLStateDiagram String Int
    }

data EnumArrowsConfig
  = EnumArrowsConfig {
      sdConfig :: SDConfig
    , maxInstances :: Maybe Integer
    , syntaxWarnTooManyArrows :: Bool
    , printExtendedFeedback :: Bool
  }

defaultEnumArrowsConfig :: EnumArrowsConfig
defaultEnumArrowsConfig
  = EnumArrowsConfig {
      sdConfig = defaultSDConfig
    , maxInstances = Just 1000
    , printExtendedFeedback = False
    , syntaxWarnTooManyArrows = False
  }

enumArrows :: MonadIO m => EnumArrowsConfig -> Int -> m EnumArrowsInstance
enumArrows config timestamp
  = evalRandT (enumArrowsInstance config) (mkStdGen timestamp)

enumArrowsTask :: (OutputMonad m, MonadIO m) => FilePath -> EnumArrowsInstance -> LangM m
enumArrowsTask path task
  = do
    paragraph $ translate $ do
      english "Consider the following state chart."
      german "Betrachten Sie folgende Zustandsdiagramm."
    image $=<< liftIO $ drawSDToFile (combine path "plain") (hierarchicalSD task)
    paragraph $ translate $ do
      english "Which was flattened, having the transition literals replaced by integers."
      german "Es wurde flachgeklopft, wobei die Transitionsliterale durch ganze Zahlen ersetzt wurden."
    image $=<< liftIO $ drawSDToFile (combine path "flattened") (flatAndEnumerated (hierarchicalSD task))
    paragraph $ translate $ do
      english "Please supply a list of tuples, where the first element is the integer label of the transition\n\
               \ and the second element is a string transition literals, seperated by commas, that are supposed\
               \ to be at that place."
      german "Bitte geben Sie eine Liste von Tupeln an, wobei das erste Element die Nummer der Transition ist\n\
             \ und das zweite Element eine Liste von Transitionsliteralen, jeweils mit einem Komma getrennt als String,\
             \ das an dieser Stelle stehen soll."
    paragraph $ translate $ do
      english "You may use the following syntax to denote the missing arrows:\n\
               \ [(1,\"a\")] is a list, referring to a single transition labelled (1) that is supposed to be the literal 'a'\n\
               \ if multiple literals are possible for a transition, please specify them separately by comma, e.g. [(1,\"a,b\")]\n\
               \ would be the list of literals for transition (1) that can be either 'a' or 'b'.\n\
               \ The order of the literals does not matter."
      german "Sie können die folgende Syntax verwenden, um die fehlenden Pfeile anzugeben:\n\
             \ [(1,\"a\")] ist eine Liste, die sich auf eine einzelne Transition mit der Nummer (1) bezieht, an welche das Literal 'a' gebunden sein soll.\n\
             \ Wenn mehrere Literale für eine Transition möglich sind, geben Sie diese bitte separat durch Komma an, z.B. [(1,\"a,b\")]\n\
             \ wäre die Liste der Literale für die Transition (1), die entweder 'a' oder 'b' sein können.\n\
             \ Die Reihenfolge der Literale spielt keine Rolle."

    pure ()
  where
  flatAndEnumerated stateChart
    = umlStateDiagram $ unUML (\name substates connection startState ->
        StateDiagram {
          name = name
        , substates = substates
        , connections = zipWith (\c l -> c {transition = (show::Int -> String) l}) connection [1..]
        , startState = startState
        , label = 999
        }
    ) (mergeEqSourceAndTarget (rename concat (flatten stateChart)))

enumArrowsInstance :: (RandomGen g, MonadIO m) => EnumArrowsConfig -> RandT g m EnumArrowsInstance
enumArrowsInstance (EnumArrowsConfig sdConfig (Just maxInstances)  _ _)
  = do
    inst <- liftIO $ getInstances (Just maxInstances) (sdConfigToAlloy 10 6 (Just "scenario1") sdConfig)
    r <- liftIO (randomRIO (0, fromIntegral maxInstances - 1) :: IO Int)
    pure (EnumArrowsInstance ( map (failWith id . parseInstance "this") inst !! r))
enumArrowsInstance _ = undefined


enumArrowsInstanceCheck :: (MonadIO m, MonadRandom m) => EnumArrowsConfig -> EnumArrowsInstance -> m (Maybe String)
enumArrowsInstanceCheck _ task
  | length (enumArrowsSolution task) > 30
    = return $ Just "The solution chart exceeds a reasonable ammout of transitions, it would be tedious to enumerate them all."
  | otherwise = return Nothing

enumArrowsSyntax :: (OutputMonad m) => EnumArrowsInstance -> [(Int,String)] -> LangM m
enumArrowsSyntax task answer
  = do
    -- todo: fix this assertion
    --assertion (not $ any (\(i,_) -> any (\(i',_) -> i' == i) answer) answer) $ translate $ do
    --  english "You can only assign one set of literals as a comma sperated string for every enumerated arrow at most once."
    --  german "Eine Menge von Literalen kann als kommaseparierter String höchstens ein Mal für einen konkreten nummerierten Pfeil zugewiesen werden."
    assertion (any (\(i,_) -> i < 1) answer) $ translate $ do
      english "Transition enumeration must be positive integers."
      german "Um eine Transition anzugeben, muss diese mit einer positiven ganzen Zahlen nummeriert werden."
    assertion ( syntaxWarnTooManyArrows defaultEnumArrowsConfig &&
                length answer > (length (enumArrowsSolution task) +
               (length (enumArrowsSolution task) `div` 2))) $ translate $ do
      english "Transition enumeration must not exceed the number of transitions in the chart."
      german "Es können nicht mehr Transitionen nummeriert werden als in der Aufgabenstellung vorhanden sind."
    assertion (any (\(_,l) -> l == "") answer) $ translate $ do
      english "Transition literals must not be empty."
      german "Transitionsliterale dürfen nicht leer sein."
    assertion (null answer) $ translate $ do
      english "Please supply a list of tuples, where the first element is the integer of the transition\n\
               \ and the second element is the transition literal as string that is supposed to be at that place."
      german "Bitte geben Sie eine Liste von Tupeln an, wobei das erste Element die Nummer der Transition ist\n\
             \ und das zweite Element eine Liste von Transitionsliteralen, jeweils mit einem Komma getrennt als String,\
             \ das an dieser Stelle stehen soll."
    return ()

-- ask siegburg about evalLangM, how to deploy this function into main() without having missing instances
enumArrowsFeedback :: (OutputMonad m, (Monad (GenericLangM Language m))) => EnumArrowsInstance -> [(Int,String)] -> LangM m
enumArrowsFeedback task answer
  = do
    mapM_ (\(i,l)
             -> (\case
                    Just l'
                      -> do
                         paragraph $ translate $ do
                           english ("at transition " ++ show i ++
                                    " you wrote " ++ show l ++
                                    ", but the correct literal is " ++ show l')
                           german ("bei Transition " ++ show i ++
                                   " haben Sie " ++ show l ++
                                   " geschrieben, aber das korrekte Literal ist " ++ show l')
                    Nothing
                      -> do
                         paragraph $ translate $ do
                           english ("the enumerated transition " ++ show i ++
                                    " is not present in the chart.")
                           german ("die nummerierte Transition " ++ show i ++
                                   " ist nicht in der Aufgabenstellung vorhanden.")
                )
      (lookup i answer) ) (filter (\x -> sortLiterals x `notElem` map sortLiterals (enumArrowsSolution task) ) answer)
    mapM_ (\(i,l) -> do
                     paragraph $ translate $ do
                       english ("the enumerated transition: " ++ show i ++
                                " is missing, the expected literal for it is: " ++ show l)
                       german ("die Transition mit der Nummer: " ++ show i ++
                               " fehlt, das dort erwartete Literal ist: " ++ show l)
          )
      (filter (isNothing . flip lookup answer . fst) (enumArrowsSolution task))
  where
  sortLiterals (a,b) -- to compare user answers with the solution
    = (,) a $ intercalate "," (sort  (filter ("," /=)  (groupBy (\x' y' -> x' /= ',' && y' /= ',') b)))


enumArrowsEvaluation :: OutputMonad m => EnumArrowsInstance -> [(Int,String)] -> Rated m
enumArrowsEvaluation task answer
  = let pts' -- grade by every single transition correctly rewired
          = toRational (sum $
            map ((\(i,ls)
                   -> sum $ map (\l
                                  -> maybe (0::Int) (\ls'
                                                      -> if l `elem` ls'
                                                         then 1
                                                         else 0)
                                    (lookup i
                                    (map
                                    (second (nub . filter ("," /=) .
                                     groupBy (\a b -> a /= ',' && b /= ',') . nub ))
                                     answer)))
                ls)
               . second (filter ("," /=)
               . groupBy (\a b -> a /= ',' && b /= ',')
               )) (enumArrowsSolution task))
            / toRational (sum $
                          map ( length . (filter ("," /=) .
                               groupBy (\a b -> a /= ',' && b /= ',')) .
                               snd )
           (enumArrowsSolution task))
    in
    printSolutionAndAssert (Just $ show $ enumArrowsSolution task) pts'

enumArrowsSolution :: EnumArrowsInstance -> [(Int,String)]
enumArrowsSolution EnumArrowsInstance {hierarchicalSD}
  = unUML (\_ _ connection _
             -> zipWith
                (\c l -> (,) l $ transition c)
                connection
                [1..]
    ) (mergeEqSourceAndTarget (rename concat (flatten hierarchicalSD)))

defaultEnumInstance :: EnumArrowsInstance
defaultEnumInstance
  = EnumArrowsInstance {
    hierarchicalSD = flatCase1
  }
