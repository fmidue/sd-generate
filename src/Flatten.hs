{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}

module Flatten (flatten
               ,flatten') where

import Datatype (UMLStateDiagram
                ,umlStateDiagram
                ,unUML
                ,StateDiagram(..)
                ,globalise
                ,Connection(..)
                ,pointFromL
                ,pointToL
                ,rename
                )
import Datatype.ClassInstances ()
import Data.Either.Extra (fromLeft')
import Data.List (find, singleton)
import Data.Bifunctor (bimap)
import Control.Lens (over, traverseOf)

flatten :: (Eq l, Enum l, Num l) => UMLStateDiagram n l -> UMLStateDiagram [n] l
flatten
 = -- localise' .
   maybe (error "not defined") distinctLabels
   . liftSD
   . fmap Left
   . globalise
   . rename singleton

flatten' :: (Eq l, Enum l, Num l) => UMLStateDiagram [n] l -> UMLStateDiagram [n] l
flatten'
 = maybe (error "not defined") distinctLabels
   . liftSD
   . fmap Left
   . globalise

{-
localise' :: (Eq l, Enum l, Num l) => UMLStateDiagram [n] l -> UMLStateDiagram [n] l
localise'
  = umlStateDiagram
    . unUML (\name substates connections startState
      -> localise
         StateDiagram { name
                      , substates
                      , connections
                      , startState
                      , label = undefined
                      }
    )
-}

liftSD :: (Eq l) => UMLStateDiagram [n] (Either l l) -> Maybe (UMLStateDiagram [n] (Either l l))
liftSD
  = fmap umlStateDiagram . unUML
    (\globalName rootNodes globalConnections globalStartState ->
      case find isHierarchical rootNodes
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
      Just _
        -> error "we dont expect anything else than StateDiagram or Nothing here"
      Nothing
        -> Nothing
    )
  where
    isState InnerMostState{} = True
    isState CombineDiagram{} = True
    isState other = isHierarchical other
    isHierarchical StateDiagram{} = True
    isHierarchical _ = False

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

distinctLabels :: (Eq l, Enum l, Num l) => UMLStateDiagram n (Either l l) -> UMLStateDiagram n l
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

matchToRelation :: (Eq a) => a -> [(a, b)] -> b
matchToRelation x r
  = case lookup x r of
     Just u
       -> u
     Nothing
       -> error "no matching label can be found"

matchNodeToRelation :: (Eq l) => [(Either l l, l)] -> StateDiagram n (Either l l) [Connection (Either b b)] -> StateDiagram n l [Connection b]
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
           ForkOrJoin { label }
             -> ForkOrJoin { label = matchToRelation label r }
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

matchConnectionToRelation :: (Eq l) => [Connection (Either l l)] -> [(Either l l, l)] -> [Connection l]
matchConnectionToRelation connections r
  = [ c { pointFrom
            = mapHeadTail (`matchToRelation` r) fromLeft' (pointFrom c)
        , pointTo
            = mapHeadTail (`matchToRelation` r) fromLeft' (pointTo c)
        } | c <- connections ]
