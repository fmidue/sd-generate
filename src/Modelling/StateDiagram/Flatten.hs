{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}

module Modelling.StateDiagram.Flatten (flatten) where

import Modelling.StateDiagram.Datatype (UMLStateDiagram
                ,umlStateDiagram
                ,unUML
                ,StateDiagram(..)
                ,globalise
                ,Connection(..)
                ,rename
                )
import Modelling.StateDiagram.Datatype.ClassInstances ()
import Modelling.StateDiagram.Datatype.Lenses (pointFromL, pointToL, pointBothL)
import Data.Either.Extra (fromLeft')
import Data.Maybe (fromJust)
import Data.List (find, singleton)
import Data.Bifunctor (bimap)
import Control.Lens (over
                    ,traverseOf)

flatten :: UMLStateDiagram n Int -> UMLStateDiagram [n] Int
flatten chart
  = go (globalise $ rename singleton chart)
  where
    go c =
      maybe c (go . distinctLabels) (lift (Left <$> c))

lift :: Eq l => UMLStateDiagram [n] (Either l l) -> Maybe (UMLStateDiagram [n] (Either l l))
lift
  = fmap umlStateDiagram . unUML
    (\globalName rootNodes globalConnections globalStartState ->
      case find isCompoundState rootNodes
      of
      Just StateDiagram { label = liftedNodeAddress
                        , substates = elevatedSubstates
                        , name = liftedName
                        , startState = liftedStartState
                        }
        -> Just (
             StateDiagram
               { name = globalName
               , startState = rewireEntering liftedNodeAddress liftedStartState
                              globalStartState
               , label = undefined
               , substates
                   = map ( inheritName liftedName
                         . \case
                             History{} -> error "History as inner node is not supported here."
                             node -> node { label = Right . fromLeft' $ label node })
                     elevatedSubstates
                     ++
                     filter ((liftedNodeAddress /=) . label) rootNodes
               , connections
                   = concatMap ( traverseOf pointFromL
                                 (rewireExiting liftedNodeAddress
                                   (map label $ filter isState elevatedSubstates))
                               . over pointToL
                                 (rewireEntering liftedNodeAddress liftedStartState) )
                     globalConnections
               }
           )
      Just CombineDiagram {}
        -> Nothing
      Just _
        -> error "we don't expect to find anything else here"
      Nothing
        -> Nothing
    )
  where
    isState InnerMostState{} = True
    isState other = isCompoundState other
    isCompoundState CombineDiagram{} = True
    isCompoundState StateDiagram{} = True
    isCompoundState _ = False

inheritName :: [n] -> StateDiagram [n] l c -> StateDiagram [n] l c
inheritName pName sd@StateDiagram { name = sdName }
  = sd { name = pName ++ sdName }
inheritName pName innerState@InnerMostState { name = innerMostName }
  = innerState { name = pName ++ innerMostName }
inheritName _ node = node

rewireEntering :: Eq b => Either b b -> [Either b b] -> [Either b b] -> [Either b b]
rewireEntering liftedNodeAddress liftedStartState to
  | [liftedNodeAddress] == to
  = mapHead (Right . fromLeft') liftedStartState
rewireEntering liftedNodeAddress _ to
  = rewireAny liftedNodeAddress to

rewireAny :: Eq b => Either b b -> [Either b b] -> [Either b b]
rewireAny liftedNodeAddress (x:xs)
  | liftedNodeAddress == x
  = mapHead (Right . fromLeft') xs
rewireAny _ point = point

rewireExiting :: Eq b => Either b b -> [Either b b] -> [Either b b] -> [[Either b b]]
rewireExiting liftedNodeAddress elevatedSubstates from
  | [liftedNodeAddress] == from
  = map (singleton . Right . fromLeft') elevatedSubstates
rewireExiting liftedNodeAddress _ from
  = [ rewireAny liftedNodeAddress from ]

distinctLabels :: Eq l => UMLStateDiagram n (Either Int l) -> UMLStateDiagram n Int
distinctLabels
  = umlStateDiagram . unUML
    (\name substates connections startState ->
       let
       r = zip
           (map label substates)
           [1..]
       in
       StateDiagram { substates
                        = map (matchNodeToRelation r) substates
                    , connections
                        = map (fmap fromLeft' . over pointBothL (mapHead (Left . lookup' r))) connections
                    , name = name
                    , startState
                        = mapHeadTail (lookup' r) fromLeft' startState
                    , label = error "not relevant"
                    }
    )

lookup' :: Eq a => [(a, b)] -> a -> b
lookup' r x = fromJust (lookup x r)

matchNodeToRelation :: (Eq l, Eq c) => [(Either l c, l)] -> StateDiagram n (Either l c) [Connection (Either b d)] -> StateDiagram n l [Connection b]
matchNodeToRelation r
      = \case
           StateDiagram { label
                        , substates
                        , startState
                        , ..
                        }
             -> StateDiagram { label
                                 = lookup' r label
                             , substates
                                 = map (bimap fromLeft' (const [])) substates
                             , connections
                                 = []
                             , startState
                                 = map fromLeft' startState
                             , .. }
           CombineDiagram { label
                          , substates
                          }
             -> CombineDiagram { label
                                   = lookup' r label
                                , substates
                                    = map (bimap fromLeft' (const [])) substates
                                }
           InnerMostState { label, .. }
             -> InnerMostState { label = lookup' r label
                               , .. }
           EndState { label }
             -> EndState { label = lookup' r label }
           Fork { label }
             -> Fork { label = lookup' r label }
           Join { label }
             -> Join { label = lookup' r label }
           History { label
                   , .. }
              -> History { label = lookup' r label
                         , .. }

mapHeadTail :: (a -> b) -> (a -> b) -> [a] -> [b]
mapHeadTail f g (x:xs) = f x : map g xs
mapHeadTail _ _ _      = error "impossible!"

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ []     = error "impossible!"
mapHead f (x:xs) = f x : xs
