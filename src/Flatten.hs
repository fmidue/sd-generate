{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns            #-}
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
import Datatype (UMLStateDiagram(unUML)
                ,umlStateDiagram
                ,StateDiagram(..)
                ,globalise
                ,Connection(..)
                )
import Data.List(groupBy
                ,sortBy
                ,sort)
import Data.Bifunctor(bimap
                     ,Bifunctor(second, first))

instance Functor UMLStateDiagram where
  fmap f = umlStateDiagram . bimap f (map (fmap f)) . unUML

flatten :: UMLStateDiagram Int -> UMLStateDiagram Int
flatten d
  = umlStateDiagram . fromFlat $ lift diagram
    where
    diagram = toFlat $ globalise d

lift :: FlatDiagram -> FlatDiagram
lift x@(StateDiagram{substate,connection})
  = case target x of
     Just StateDiagram { label
                       , startState
                       , substate = inner }
       -> let
          address = label
          initial = map (\(Left y) -> Right y) startState
          in
          x { substate
                = inner ++
                  filter (\case
                            InnerMostState {}
                              -> True
                            _ -> False
                         ) substate
            , connection = rewire connection address initial }
     Nothing
       -> error "scenario1 expects at least one hierarchical state"

{- we could use a Maybe to handle the possiblity of a reduced scenario1
   i.e. no hierarchical states at all to avoid failing there            -}
target :: FlatDiagram -> Maybe FlatDiagram
target
  = \case
      (StateDiagram {substate})
        -> let
           sd = filter (\case
                   StateDiagram {}
                     -> True
                   _ -> False ) substate
           in
           if not (null sd)
           then Just (head sd)
           else Nothing

rewire :: [FlatConnection] -> Either Int Int -> [Either Int Int] -> [FlatConnection]
rewire r _ _
  = r

type FlatConnection = Connection (Either Int Int)

type FlatDiagram = StateDiagram (Either Int Int) [FlatConnection]

toFlat :: UMLStateDiagram Int -> FlatDiagram
toFlat
  = unUML . fmap Left

fromFlat :: FlatDiagram -> StateDiagram Int [Connection Int]
fromFlat =
        (\case
            (StateDiagram { label = Right newLabel
                          , substate
                          , name
                          , connection
                          , startState })
              -> (StateDiagram { label = newLabel
                               , substate = map fromFlat substate
                               , name = name
                               , connection = map (fmap (\case Left x -> x)) connection
                               , startState = map (\case Left x -> x) startState
                               })
            (InnerMostState { label = Right newLabel
                            , name
                            , operations })
              -> (InnerMostState { label = newLabel
                                 , name = name
                                 , operations = operations })
            _ -> error "not supported"
         )
