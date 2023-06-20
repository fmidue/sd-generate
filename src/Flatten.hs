{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE InstanceSigs              #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Flatten (
 flatten
) where
import Datatype (UMLStateDiagram
                ,StateDiagram'(..)
                ,StateDiagram
                ,globalise
                ,Connection'(..)
                ,Connection
                )
import Data.List(groupBy
                ,sortBy
                , sort)
import Data.Bifunctor(bimap
                     ,Bifunctor(second, first))

flatten :: UMLStateDiagram -> UMLStateDiagram
flatten = globalise

type FlatConnection = Connection' [Either Int Int] [Either Int Int]

type FlatDiagram = StateDiagram' (Either Int Int) [FlatConnection]

target :: [FlatDiagram] -> Maybe FlatDiagram
target substate
  = if not (null sd)
    then Just (head sd)
    else Nothing
    where
    sd = filter (\case
                   StateDiagram {}
                     -> True
                   _ -> False ) substate

convertToFlat :: UMLStateDiagram -> FlatDiagram
convertToFlat x
  = head $ convertDiagramToFlat [x]

{- all Either values have to be Right, otherwise the conversion will fail -}
convertFromFlat :: FlatDiagram -> UMLStateDiagram
convertFromFlat x
  = head $ convertDiagramFromFlat [x]

convertDiagramToFlat :: [UMLStateDiagram] -> [FlatDiagram]
convertDiagramToFlat
  = map (\case
            (InnerMostState{ label
                           , name
                           , operations })
              -> (InnerMostState { label = Left label
                                 , name = name
                                 , operations = operations
                                 })
            (StateDiagram{ label
                         , substate
                         , name
                         , connection
                         , startState })
              -> (StateDiagram { label = Left label
                               , substate = convertDiagramToFlat substate
                               , name = name
                               , connection = convertConnectionToFlat connection
                               , startState = map Left startState })
            _ -> error "not supported"
         )

convertConnectionToFlat :: [Connection] -> [FlatConnection]
convertConnectionToFlat
  = map (\case
            (Connection{ pointFrom
                       , pointTo
                       , transition })
              -> Connection { pointFrom = map Left pointFrom
                            , pointTo = map Left pointTo
                            , transition = transition })

convertDiagramFromFlat :: [FlatDiagram] -> [UMLStateDiagram]
convertDiagramFromFlat
  = map (\case
            (StateDiagram { label = Right newLabel
                          , substate
                          , name
                          , connection
                          , startState })
              -> (StateDiagram { label = newLabel
                               , substate = convertDiagramFromFlat substate
                               , name = name
                               , connection = convertConnectionFromFlat connection
                               , startState = foldr (\case Left x -> (x:) ) [] startState
                               })
            (InnerMostState { label = Right newLabel
                            , name
                            , operations })
              -> (InnerMostState { label = newLabel
                                 , name = name
                                 , operations = operations })
            _ -> error "not supported"
         )

convertConnectionFromFlat :: [FlatConnection] -> [Connection]
convertConnectionFromFlat
  = map (\case
            (Connection { pointFrom
                        , pointTo
                        , transition
                        })
              -> (Connection { pointFrom = foldr (\case Left x -> (x:)) [] pointFrom
                             , pointTo = foldr (\case Left x -> (x:)) [] pointTo
                             , transition = transition
                             })
         )
