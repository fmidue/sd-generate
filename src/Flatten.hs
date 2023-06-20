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

type FlatConnection = Connection' (Either [Int] [Int]) (Either [Int] [Int])

type FlatDiagram = StateDiagram' (Either Int Int) (Either [Int] [Int]) [FlatConnection]

findSD
  = head .
    filter (\case
              StateDiagram {substate}
                -> True
              _ -> False )

convertDiagram :: [UMLStateDiagram] -> [FlatDiagram]
convertDiagram = fmap (\case
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
                               , substate = convertDiagram substate
                               , name = name
                               , connection = convertConnection connection
                               , startState = Left startState })
            _ -> error "not supported"
         )

convertConnection :: [Connection] -> [FlatConnection]
convertConnection
  = fmap (\case
            (Connection{ pointFrom
                       , pointTo
                       , transition })
              -> Connection { pointFrom = Left pointFrom
                            , pointTo = Left pointTo
                            , transition = transition })

convert :: UMLStateDiagram -> FlatDiagram
convert x = head $ convertDiagram [x]

