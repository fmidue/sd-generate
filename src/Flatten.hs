{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving, DerivingVia #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE LambdaCase #-}

module Flatten (
  flatten
) where
import Datatype (UMLStateDiagram
                ,umlStateDiagram
                ,unUML
                ,StateDiagram(..)
                ,globalise
                ,Connection(..)
                )

import Generic.Functor (GenericFunctor(..))
import Data.Either.Extra (fromLeft')

deriving via GenericFunctor UMLStateDiagram instance Functor UMLStateDiagram

flatten :: UMLStateDiagram Int -> UMLStateDiagram Int
flatten d
 = umlStateDiagram . fromFlat $ unUML lift (fmap Left (globalise d))
   where
   lift name substate connection outerStartState =
    case target substate of
     Just StateDiagram { label
                       , startState
                       , substate = inner }
       -> let
          address = label
          initial
            = map (Right . fromLeft') startState
          in
          StateDiagram
            { name = name
            , startState = outerStartState
            , label = Left $ error "There seems no good reason why this outermost label should be relevant. TODO: fix conversion impl."
            , substate
                = map (\case
                         i@InnerMostState{ name = innerName
                                        , label = Left innerLabel }
                           -> i { name = name ++ "_" ++ innerName
                                , label = Right innerLabel }
                         _ -> error "scenario1 only expects InnerMostStates as substates of a StateDiagram"
                      ) inner
                  ++
                  filter (\case
                            InnerMostState {}
                              -> True
                            _ -> False
                         ) substate
            , connection = rewire connection address initial inner }
     Just _
       -> error "we dont exepect anything else than StateDiagram or Nothing here"
     Nothing
       -> error "scenario1 expects at least one hierarchical state"

target :: [FlatDiagram] -> Maybe FlatDiagram
target substate
         = let
           sd = filter (\case
                   StateDiagram {}
                     -> True
                   _ -> False ) substate
           in
           if not (null sd)
           then Just (head sd)
           else Nothing

rewire :: [FlatConnection] -> Either Int Int -> [Either Int Int] -> [FlatDiagram] -> [FlatConnection]
rewire connections address initial inner
  = map (updateLifted address initial) $
    concatMap (updateCompoundExits address inner) connections

updateByRule :: Either Int Int -> [Either Int Int] -> [Either Int Int] -> [Either Int Int]
updateByRule address initial [x]
  | x == address = map (\case
                          Left y -> Right y
                          Right y -> Right y) initial
updateByRule address _ (x:xs)
  | x == address = map (Right . fromLeft') xs
updateByRule _ _ labels = labels

updateLifted :: Either Int Int -> [Either Int Int] -> FlatConnection -> FlatConnection
updateLifted address initial c@(Connection{pointFrom,pointTo})
  = c { pointFrom = updateByRule address initial pointFrom
      , pointTo = updateByRule address initial pointTo }

updateCompoundExits :: Either Int Int -> [FlatDiagram] -> FlatConnection -> [FlatConnection]
updateCompoundExits address inner c@Connection{ pointFrom
                                              , pointTo
                                              , transition }
  | pointFrom == [address]
  = [ Connection { pointFrom
                     = [(\case
                           Left y -> Right y
                           _ -> error "input should be Left" ) label]
                 , pointTo = pointTo
                 , transition = transition
                 } | InnerMostState{label} <- inner ]
  | otherwise = [c]

type FlatConnection = Connection (Either Int Int)

type FlatDiagram = StateDiagram (Either Int Int) [FlatConnection]

fromFlat :: FlatDiagram -> StateDiagram Int [Connection Int]
fromFlat =
        \case
            (StateDiagram { label = newLabel
                          , substate
                          , name
                          , connection
                          , startState })
              -> (StateDiagram { label
                                   = (\case
                                        Right x -> x
                                        Left x -> x
                                     ) newLabel
                               , substate = map fromFlat substate
                               , name = name
                               , connection
                                   = map (fmap (\case
                                                  Left x -> x
                                                  Right x -> x)
                                        ) connection
                               , startState
                                   = map (\case
                                            Left x -> x
                                            Right x -> x
                                         ) startState
                               })
            (InnerMostState { label = newLabel
                            , name
                            , operations })
              -> (InnerMostState { label
                                     = (\case
                                          Right x -> x
                                          Left x -> x
                                       ) newLabel
                                 , name = name
                                 , operations = operations })
            _ -> error "not supported"
