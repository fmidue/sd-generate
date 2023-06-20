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
