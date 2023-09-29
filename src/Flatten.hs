{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE LambdaCase                #-}

module Flatten (
  flatten
) where

import Datatype (UMLStateDiagram
                ,umlStateDiagram
                ,unUML
                ,StateDiagram(..)
                ,globalise
                ,Connection(..)
                ,rename, HistoryType (..)
                )
import Datatype.ClassInstances ()
import Data.Either.Extra (fromLeft')
import Data.List (find)

flatten :: (Num l, Enum l, Eq l, Show l)
  => UMLStateDiagram n l -> UMLStateDiagram [n] l
flatten
 = maybe (error "not defined") distinctLabels  -- using an is flat predicate would allow to loop over the lifting process until the chart is flat (max times nr. of liftable nodes)
   . lift
   . fmap Left
   . globalise
   . rename (:[])

lift :: (Eq l)
  => UMLStateDiagram [n] (Either l l) -> Maybe (UMLStateDiagram [n] (Either l l))
lift
  = fmap umlStateDiagram . unUML
    (\outerName outerStates connections outerStartState ->
      case find (\case
                   StateDiagram {}
                     -> True
                   _ -> False) outerStates
      of
     Just StateDiagram { label = address
                       , startState = innerStartState
                       , substates = innerStates
                       , name = parentName }
       -> let
          initial
            = map (Right . fromLeft') innerStartState 
            {- TODO: maybe provide better error handling,
                           in case of the presence of implicit entries towards the StateDiagram
                           with connections targeting this child,
                           but without an initial state set in it.
            -}
          in
          Just (
            StateDiagram
              { name
                  = outerName
              , startState
                  = updateByRule address initial outerStartState
              , label
                  = undefined
              , substates
                  = map (\case
                           i@InnerMostState{ name = innerName
                                           , label = Left innerLabel }
                             -> i { name = parentName ++ innerName
                                  , label = Right innerLabel }
                           s@StateDiagram{ name = innerSDName
                                         , label = Left innerSDLabel
                                         , startState = innerSDStartState
                                         , substates = innerSDSubstates 
                                         , connections = [] }
                             -> s { name = parentName ++ innerSDName
                                  , label = Right innerSDLabel
                                  , startState = map (Right . fromLeft') innerSDStartState
                                  , substates = labelRight innerSDSubstates
                                  , connections = []
                                  }
                           StateDiagram{ connections = _} -> error "error; a non globalized chart was supplied"
                           e@EndState{ label = Left innerLabel }
                             -> e { label = Right innerLabel }
                           c@CombineDiagram{ label = Left innerLabel 
                                           , substates = innerCDSubstates }
                             -> c { label = Right innerLabel 
                                  , substates = labelRight innerCDSubstates }
                           f@ForkOrJoin{ label = Left innerLabel } -> f { label = Right innerLabel }
                           {- the really interesting part here will be lifting histories,
                              as they are not just relabeled,
                              but they need to be resolved from within the
                              StateDiagram they are part of, following the
                              outgoing transitions available to exit the region
                              (shallow histories should be done first as they are simpler)
                           -}
                           History{ label = Left _
                                  , historyType = Shallow } -> error "cannot handle shallow histories yet"
                           History{ label = Left _
                                  , historyType = Deep } -> error "cannot handle deep histories yet"
                           _ -> error "constructor not covered to lift upwards; or likely a bad labeling"
                        ) innerStates
                    ++
                    filter ((address /=) . label) outerStates  -- dont take the lifted vertex with us again
              , connections
                {- rewire the connections, by patching up the lifted
                   nodes; being Right now, done accordingly with replacing previous
                   Left labeled paths to them.
                   Routes to Left nodes should remain as they are. -}
                  = rewire connections address initial
                      (map (fromLeft' . label) innerStates)
              }
               )
     Just _
       -> error "we dont expect anything else than StateDiagram or Nothing here"
     Nothing
       -> Nothing
    )

 -- not good, beware; i just want to update the record... to Right, but there must be a better way
 -- ~quote; functions calls shouldn't be just containing a map (as it can be expr. in place instead)
labelRight :: [StateDiagram n (Either r r) a] -> [StateDiagram n (Either r r) a]
labelRight = map (\x -> x { label = (Right . fromLeft' . label ) x })

-- knowing that fromEither exists, i need a traversal through the chart unpacking the labels
-- that didnt change and can still be referenced; TODO: fromLeft might be totally sufficient here
fromEither' :: StateDiagram n (Either b c) [Connection (Either b c)] -> StateDiagram n b [Connection b]
fromEither' (StateDiagram { name
                          , label
                          , substates
                          , startState
                          , connections = [] })
  = StateDiagram { name
                 , label = fromLeft' label
                 , substates = map fromEither' substates -- TODO: fix that, left should be enough
                 , startState = map fromLeft' startState
                 , connections = [] }
fromEither' (CombineDiagram { label
                              , substates })
  = CombineDiagram { label = fromLeft' label
                   , substates = map fromEither' substates }
fromEither' (EndState { label })
  = EndState { label = fromLeft' label }
fromEither' (ForkOrJoin { label })
  = ForkOrJoin { label = fromLeft' label }
fromEither' (History { label
                       , historyType })
  = History { label = fromLeft' label
            , historyType }
fromEither' (InnerMostState { name
                            , label 
                            , operations})
  = InnerMostState { name
                   , label = fromLeft' label
                   , operations }
fromEither' _ = error "failed to extract int label"

{- connections; is the set of all connections in the chart (globalized form)
   address; is the label of the vertex lifted
   initial; is the initial state label present in the vertex lifted
   innerExits; are the labels of the lifted vertices substates
-}
rewire :: Eq l
  => [Connection (Either l l)] -> Either l l -> [Either l l] -> [l] -> [Connection (Either l l)]
rewire connections address initial innerExits
  = map (updateLifted address initial) $
    concatMap (updateCompoundExits address innerExits) connections

updateByRule :: Eq l
  => Either l l -> [Either l l] -> [Either l l] -> [Either l l]
updateByRule address initial [x]  -- implicit entry rule
  | x == address = initial
updateByRule address _ (x:xs)  -- update transitions targeting the substates of the lifted vertex
  | x == address = map (Right . fromLeft') xs
updateByRule _ _ labels = labels  -- don't alter Left addresses not pointing to moved vertices that became Right

updateLifted :: Eq l
  => Either l l -> [Either l l] -> Connection (Either l l) -> Connection (Either l l)
updateLifted address initial c@(Connection{pointFrom,pointTo})
  = c { pointFrom = updateByRule address initial pointFrom
      , pointTo = updateByRule address initial pointTo }

updateCompoundExits :: (Eq l, Eq r)
  => Either l r -> [r] -> Connection (Either l r) -> [Connection (Either l r)]
updateCompoundExits address innerExits c@Connection{ pointFrom
                                                   , pointTo
                                                   , transition }
  | pointFrom == [address]
  = [ Connection { pointFrom
                     = [Right label]
                 , pointTo = pointTo
                 , transition = transition
                 } | label <- innerExits ]
  | otherwise = [c]

distinctLabels :: (Eq b, Show b, Num l, Enum l, Eq l, Show l)
  => UMLStateDiagram a (Either l b) -> UMLStateDiagram a l
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
                        = map (`matchToRelation` r) startState
                    , label = error "not relevant"
                    }
    )

matchToRelation :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
matchToRelation x r
  = case lookup x r of
     Just u
       -> u
     Nothing
       -> error $ "no matching label can be found for " ++ show x ++ " while updating using " ++ show r

matchNodeToRelation :: (Eq c, Eq b, Show c, Show b)
  => [(Either b c, b)] -> StateDiagram n (Either b c) [Connection (Either b c)] -> StateDiagram n b [Connection b]
matchNodeToRelation r
      = \case
           InnerMostState{ label, name, operations }
             -> InnerMostState { label
                                   = matchToRelation label r
                                , name = name
                                , operations = operations
                               }
           StateDiagram { label
                        , substates
                        , name
                        , startState
                        }
             -> StateDiagram { label
                                 = matchToRelation label r
                             , name
                                 = name
                             , substates
                                 = map fromEither' substates
                             , connections
                                 = []
                             , startState
                                 = map fromLeft' startState }
           _ -> error "not covered constructor to match relation against"

mapHeadTail :: (a -> b) -> (a -> b) -> [a] -> [b]
mapHeadTail f g (x:xs) = f x : map g xs
mapHeadTail _ _ _      = error "impossible!"

-- only update head, because the references to the tail remain valid
matchConnectionToRelation :: (Eq b, Eq c, Show b, Show c)
  => [Connection (Either b c)] -> [(Either b c, b)] -> [Connection b]
matchConnectionToRelation connections r
  = [ c { pointFrom
            = mapHeadTail (`matchToRelation` r) fromLeft' (pointFrom c)
        , pointTo
            = mapHeadTail (`matchToRelation` r) fromLeft' (pointTo c)
        } | c <- connections ]

