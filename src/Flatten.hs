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
import Data.List (find, stripPrefix, isPrefixOf)
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
 i dont exactly understand the reasoning behind the desire to localise
 a diagram after flattening it,
 since the final result of the (entire) procedure should be a diagram with
 only one layer anyway.
 but it could be helpful in debug printouts - because stuff is more
 organized in that case

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
      case find (\case
                   StateDiagram {}
                     -> True
                   _ -> False) rootVertices
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
                   = map
                     ( inheritName liftedName
                     . (\node -> node { label = Right . fromLeft' $ label node })
                     )
                     elevatedSubstates
                     ++
                     filter ((liftedVertexAddress /=) . label) rootVertices
               , connections
                   = rewire liftedVertexAddress liftedStartState globalConnections
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

rewire :: (Eq l) => Either l l -> [Either l l] -> [Connection (Either l l)] -> [Connection (Either l l)]
rewire liftedVertexAddress liftedStartState
  = map $
      explicitSDExit liftedVertexAddress
    . explicitSDEntry liftedVertexAddress
    . implicitSDEntry liftedVertexAddress liftedStartState

rewireGlobalStartState :: (Eq l) => Either l l -> [Either l l] -> [Either l l] -> [Either l l]
rewireGlobalStartState _ _ []
  = error "invalid global start state (not present)"
rewireGlobalStartState liftedVertexAddress liftedStartState globalStartState
  = pointTo (head $ rewire
                    liftedVertexAddress liftedStartState
                    [Connection { pointTo = globalStartState
                                , pointFrom = []
                                , transition = undefined }])

explicitSDExit :: (Eq l) => Either l l -> Connection (Either l l) -> Connection (Either l l)
explicitSDExit liftedVertexAddress connection
  = if [liftedVertexAddress] `isPrefixOf` pointFrom connection
       && length (pointFrom connection) > 1
    then connection { pointFrom
                        =  maybe (error "")
                                 (mapHead (Right . fromLeft'))
                                 (stripPrefix [liftedVertexAddress] $ pointFrom connection)
                    }
    else connection

explicitSDEntry :: (Eq l) => Either l l -> Connection (Either l l) -> Connection (Either l l)
explicitSDEntry liftedVertexAddress connection
  = if [liftedVertexAddress] `isPrefixOf` pointTo connection
       && length (pointTo connection) > 1
    then connection { pointTo
                        =  maybe (error "")
                                 (mapHead (Right . fromLeft'))
                                 (stripPrefix [liftedVertexAddress] (pointTo connection))
                    }
    else connection

implicitSDEntry :: (Eq l) => Either l l -> [Either l l] -> Connection (Either l l) -> Connection (Either l l)
implicitSDEntry liftedVertexAddress liftedStartState connection
  = if pointTo connection == [liftedVertexAddress]
    then connection { pointTo
                        =  if null liftedStartState
                           then []
                           else mapHead (Right. fromLeft') liftedStartState
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
                        = mapHeadTail (`matchToRelation` r) fromLeft' startState
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
