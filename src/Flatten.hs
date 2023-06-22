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

flatten :: UMLStateDiagram Int -> UMLStateDiagram Int
flatten d
  = umlStateDiagram . fromFlat $ lift diagram
    where
    diagram = toFlat . unUML $ globalise d

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

toFlat :: StateDiagram Int [Connection Int] -> FlatDiagram
toFlat x
  = head $ diagramToFlat [x]

fromFlat :: FlatDiagram -> StateDiagram Int [Connection Int]
fromFlat x
  = head $ diagramFromFlat [x]

diagramToFlat :: [StateDiagram Int [Connection Int]] -> [FlatDiagram]
diagramToFlat
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
                               , substate = diagramToFlat substate
                               , name = name
                               , connection = connectionToFlat connection
                               , startState = map Left startState })
            _ -> error "not supported"
         )

connectionToFlat :: [Connection Int] -> [FlatConnection]
connectionToFlat
  = map (\case
            (Connection{ pointFrom
                       , pointTo
                       , transition })
              -> Connection { pointFrom = map Left pointFrom
                            , pointTo = map Left pointTo
                            , transition = transition })

diagramFromFlat :: [FlatDiagram] -> [StateDiagram Int [Connection Int]]
diagramFromFlat
  = map (\case
            (StateDiagram { label = Right newLabel
                          , substate
                          , name
                          , connection
                          , startState })
              -> (StateDiagram { label = newLabel
                               , substate = diagramFromFlat substate
                               , name = name
                               , connection = connectionFromFlat connection
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

connectionFromFlat :: [FlatConnection] -> [Connection Int]
connectionFromFlat
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
