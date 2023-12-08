{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}

module Modelling.StateDiagram.Flatten(flatten
                                     ,flatten'
                                     ,lift) where

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
import Data.List (find, singleton)
import Data.Bifunctor (bimap)
import Control.Lens (over
                    ,traverseOf)

flatten :: (Eq l, Enum l, Num l, Show l) => UMLStateDiagram n l -> UMLStateDiagram [n] l
flatten chart
  = go (globalise $ rename singleton chart)
  where
    go c =
      maybe (checkOutcome c) (go . distinctLabels) (lift (Left <$> c))
    checkOutcome c =
      unUML (\_ substates _ _ -> if all isFlatState substates
                                 then c
                                 else error "flattening failed") c
    isFlatState InnerMostState{} = True
    isFlatState EndState{} = True
    isFlatState _ = False

flatten' :: (Eq l, Enum l, Num l, Show l) => UMLStateDiagram [n] l -> UMLStateDiagram [n] l
flatten'
 = maybe (error "not defined") distinctLabels
   . lift
   . fmap Left
   . globalise

lift :: (Eq l) => UMLStateDiagram [n] (Either l l) -> Maybe (UMLStateDiagram [n] (Either l l))
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
        -> error "This is where interesting stuff for scenario2 would have to happen."
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

distinctLabels :: (Eq l, Enum l, Num l, Show l) => UMLStateDiagram n (Either l l) -> UMLStateDiagram n l
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
                        = matchConnectionToRelation connections r
                    , name = name
                    , startState
                        = mapHeadTail (`matchToRelation` r) fromLeft' startState
                    , label = error "not relevant"
                    }
    )

matchToRelation :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
matchToRelation x r
  = case lookup x r of
     Just u
       -> u
     Nothing
       -> error ("no matching label can be found for " ++ show x ++ " in " ++ show r)

matchNodeToRelation :: (Eq l, Show l) => [(Either l l, l)] -> StateDiagram n (Either l l) [Connection (Either b b)] -> StateDiagram n l [Connection b]
matchNodeToRelation r
      = \case
           StateDiagram { label
                        , substates
                        , startState
                        , ..
                        }
             -> StateDiagram { label
                                 = matchToRelation label r
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
                                   = matchToRelation label r
                                , substates
                                    = map (bimap fromLeft' (const [])) substates
                                }
           InnerMostState { label, .. }
             -> InnerMostState { label = matchToRelation label r
                               , .. }
           EndState { label }
             -> EndState { label = matchToRelation label r }
           Fork { label }
             -> Fork { label = matchToRelation label r }
           Join { label }
             -> Join { label = matchToRelation label r}
           History { label
                   , .. }
              -> History { label = matchToRelation label r
                         , .. }

mapHeadTail :: (a -> b) -> (a -> b) -> [a] -> [b]
mapHeadTail f g (x:xs) = f x : map g xs
mapHeadTail _ _ _      = error "impossible!"

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ []     = error "impossible!"
mapHead f (x:xs) = f x : xs

matchConnectionToRelation :: (Eq l, Show l) => [Connection (Either l l)] -> [(Either l l, l)] -> [Connection l]
matchConnectionToRelation connections r
  = map (fmap fromLeft' . over pointBothL (mapHead (Left . flip matchToRelation r))) connections
