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
                ,rename
                )
import Datatype.ClassInstances ()
import Data.Either.Extra (fromLeft')
import Data.List (find)
import Data.Bifunctor (bimap)

flatten :: (Eq l, Enum l, Num l) => UMLStateDiagram n l -> UMLStateDiagram [n] l
flatten
 = -- localise' .
   maybe (error "not defined") distinctLabels
   . liftSD
   . fmap Left
   . globalise
   . rename (:[])

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
    (\globalName rootVertices globalConnections globalStartState ->
      case find isHierarchical rootVertices
      of
      Just StateDiagram { label = liftedVertexAddress
                        , substates = elevatedSubstates
                        , name = liftedName
                        , startState = liftedStartState
                        }
        -> Just (
             StateDiagram
               { name = globalName
               , startState = rewireGlobalStartState liftedVertexAddress liftedStartState globalStartState
               , label = undefined
               , substates
                   = map ( inheritName liftedName
                         . (\node -> node { label = Right . fromLeft' $ label node }) )
                     elevatedSubstates
                     ++
                     filter ((liftedVertexAddress /=) . label) rootVertices
               , connections
                   = concatMap ( rewireExiting liftedVertexAddress
                                               ( map label $ filter isState elevatedSubstates )
                               . rewireEntering liftedVertexAddress liftedStartState )
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
inheritName pName innerState@InnerMostState { name = imsName }
  = innerState { name = pName ++ imsName }
inheritName _ node = node

rewireGlobalStartState :: (Eq l) => Either l l -> [Either l l] -> [Either l l] -> [Either l l]
rewireGlobalStartState liftedVertexAddress liftedStartState globalStartState
  | [liftedVertexAddress] == globalStartState
    = mapHead (Right. fromLeft') liftedStartState
rewireGlobalStartState liftedVertexAddress _ (x:xs)
  | liftedVertexAddress == x = mapHead (Right. fromLeft') xs
rewireGlobalStartState _ _ globalStartState = globalStartState

rewireEntering :: (Eq b) => Either b b -> [Either b b] -> Connection (Either b b) -> Connection (Either b b)
rewireEntering liftedVertexAddress liftedStartState connection
  | [liftedVertexAddress] == pointTo connection
    = connection { pointTo = mapHead (Right . fromLeft') liftedStartState }
rewireEntering liftedVertexAddress _ connection@Connection { pointTo = (x:xs) }
  | liftedVertexAddress == x
  = connection { pointTo = mapHead (Right . fromLeft') xs }
rewireEntering _ _ connection = connection

rewireExiting :: (Eq b) => Either b b -> [Either b b] -> Connection (Either b b) -> [Connection (Either b b)]
rewireExiting liftedVertexAddress elevatedSubstates connection
  | [liftedVertexAddress] == pointFrom connection
    = [ connection { pointFrom = [pf] } | pf <- map (Right . fromLeft') elevatedSubstates ]
rewireExiting liftedVertexAddress _ connection@Connection { pointFrom = (x:xs) }
  | liftedVertexAddress == x = [ connection { pointFrom = mapHead (Right . fromLeft') xs } ]
rewireExiting _ _ connection = [connection]

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
