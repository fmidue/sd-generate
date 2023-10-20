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
import Data.Either.Extra (fromLeft'
                         ,fromEither)
import Data.List (find, stripPrefix)
import Data.Bifunctor (bimap)
import Checkers.Helpers (getAllElem)

flatten :: (Eq l, Enum l, Num l) => UMLStateDiagram n l -> UMLStateDiagram [n] l
flatten
 = maybe (error "not defined") distinctLabels
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

liftSD :: (Eq l) => UMLStateDiagram [n] (Either l l) -> Maybe (UMLStateDiagram [n] (Either l l))
liftSD
  = fmap umlStateDiagram . unUML
    (\globalName rootVertices globalConnections globalStartState ->
      case find (\case
                   StateDiagram {}
                     -> True
                   _ -> False) rootVertices
      of
      Just liftedVertex@StateDiagram { label = liftedVertexAddress
                                     , substates = elevatedSubstates
                                     , name = liftedName }
        -> Just (
             StateDiagram
               { name = globalName
               , startState = rewireGlobalStartState liftedVertexAddress liftedVertex globalStartState
               , label = undefined
               , substates
                   = map
                     ( inheritName liftedName
                     . (\node -> node { label = Right . fromLeft' $ label node })
                     )
                     elevatedSubstates
                     ++
                     filter ((liftedVertexAddress /=) . label) rootVertices
               , connections
                   = rewire liftedVertexAddress liftedVertex globalConnections
               }
           )
      Just _
        -> error "we dont expect anything else than StateDiagram or Nothing here"
      Nothing
        -> Nothing
    )

inheritName :: [n] -> StateDiagram [n] l c -> StateDiagram [n] l c
inheritName pName sd@StateDiagram { name = sdName }
  = sd { name = pName ++ sdName }
inheritName pName innerState@InnerMostState { name = imsName }
  = innerState { name = pName ++ imsName }
inheritName _ node = node

rewire :: (Eq l) => Either l l -> StateDiagram n (Either l l) [Connection (Either l l)] -> [Connection (Either l l)] -> [Connection (Either l l)]
rewire liftedVertexAddress liftedVertex@StateDiagram{}
  = map $
      explicitSDExit [liftedVertexAddress] liftedVertex
    . explicitSDEntry [liftedVertexAddress] liftedVertex
    . implicitSDEntry [liftedVertexAddress] liftedVertex
rewire _ _ = error "not defined"

rewireGlobalStartState :: (Eq l) => Either l l -> StateDiagram n (Either l l) [Connection (Either l l)] -> [Either l l] -> [Either l l]
rewireGlobalStartState _ _ []
  = error "invalid global start state (not present)"
rewireGlobalStartState liftedVertexAddress liftedVertex@StateDiagram{} startState
  = pointTo (head $ rewire
                    liftedVertexAddress liftedVertex
                    [Connection { pointTo = startState
                                , pointFrom = []
                                , transition = undefined }])
rewireGlobalStartState _ _ _ = error "not defined"

explicitSDExit :: (Eq l) => [Either l l] -> StateDiagram n (Either l l) [Connection (Either l l)] -> Connection (Either l l) -> Connection (Either l l)
explicitSDExit liftedVertexAddress liftedVertex connection
  = if pointFrom connection
       `elem`
       map (liftedVertexAddress ++) (getAllElem liftedVertex)
    then connection { pointFrom
                        =  maybe (error "")
                                 (map (Right . fromLeft'))
                                 (stripPrefix liftedVertexAddress $ pointFrom connection)
                    }
    else connection

explicitSDEntry :: (Eq l) => [Either l l] -> StateDiagram n (Either l l) [Connection (Either l l)] -> Connection (Either l l) -> Connection (Either l l)
explicitSDEntry liftedVertexAddress liftedVertex connection
  = if pointTo connection
       `elem`
       map (liftedVertexAddress ++) (getAllElem liftedVertex)
    then connection { pointTo
                        =  maybe (error "")
                                 (map (Right . fromLeft'))
                                 (stripPrefix liftedVertexAddress (pointTo connection))
                    }
    else connection

implicitSDEntry :: (Eq l) => [Either l l] -> StateDiagram n (Either l l) [Connection (Either l l)] -> Connection (Either l l) -> Connection (Either l l)
implicitSDEntry liftedVertexAddress liftedVertex connection
  = if pointTo connection == liftedVertexAddress
    then connection { pointTo
                        = map (Right . fromLeft')
                              (startState liftedVertex)
                    }
    else connection

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
                        = mapHeadTail (`matchToRelation` r) fromEither startState
                    , label = error "not relevant"
                    }
    )

matchToRelation :: Eq a => a -> [(a, b)] -> b
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
                                 = map (bimap fromEither (const [])) substates
                             , connections
                                 = []
                             , startState
                                 = map fromEither startState
                             , .. }
           CombineDiagram { label
                          , substates
                          }
             -> CombineDiagram { label
                                   = matchToRelation label r
                                , substates
                                    = map (bimap fromEither (const [])) substates
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

matchConnectionToRelation :: (Eq l) => [Connection (Either l l)] -> [(Either l l, l)] -> [Connection l]
matchConnectionToRelation connections r
  = [ c { pointFrom
            = mapHeadTail (`matchToRelation` r) fromEither (pointFrom c)
        , pointTo
            = mapHeadTail (`matchToRelation` r) fromEither (pointTo c)
        } | c <- connections ]
