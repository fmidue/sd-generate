{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Modelling.StateDiagram.Flatten(flatten
                                     ,flatten'
                                     ,lift) where

import Modelling.StateDiagram.Datatype (UMLStateDiagram
                ,umlStateDiagram
                ,unUML
                ,StateDiagram(..)
                ,globalise
                ,Connection(..)
                ,pointFromL
                ,pointToL
                ,rename
                )
import Modelling.StateDiagram.Datatype.ClassInstances ()
import Data.Either.Extra (fromLeft', mapRight, fromRight')
import Data.List ( find, singleton, uncons, groupBy, nub )
import Data.Bifunctor (bimap)
import Control.Lens (over
                    ,traverseOf)
import Data.Set (fromList, toList, cartesianProduct)
import Data.List.Extra (sortBy)
import Data.Maybe (fromMaybe)


-- remove
class TrifunctorW g z where
  trimapW :: (a -> b) -> (c -> d) -> (e -> f) -> g a c [z e] -> g b d [z f]

instance TrifunctorW StateDiagram Connection where
  trimapW :: (a -> b) -> (c -> d) -> (e -> f) -> StateDiagram a c [Connection e] -> StateDiagram b d [Connection f]
  trimapW a b c StateDiagram { name, substates, connections, startState, label }
    = StateDiagram { name = a name
                   , substates = map (trimapW a b c) substates
                   , connections = map (fmap c) connections
                   , startState = map b startState
                   , label = b label
                   }
  trimapW a b c CombineDiagram { substates, label }
    = CombineDiagram { substates = map (trimapW a b c) substates
                     , label = b label
                     }
  trimapW a b _ InnerMostState { name, operations, label }
    = InnerMostState { name = a name
                     , operations = operations
                     , label = b label
                     }
  trimapW _ b _ EndState { label }
    = EndState { label = b label }
  trimapW _ b _ Fork { label }
    = Fork { label = b label }
  trimapW _ b _ Join { label }
    = Join { label = b label }
  trimapW _ b _ History { label, historyType }
    = History { label = b label
              , historyType = historyType }

flatten :: (Eq l, Enum l, Num l, Ord l, Ord n, Show l) => UMLStateDiagram n l -> UMLStateDiagram [n] l
flatten chart
  = go (globalise $ rename singleton chart)
  where
    go c =
      maybe (checkOutcome c) go (distinctLabels <$> (lift (Left <$> c)))
    checkOutcome c =
      unUML (\_ substates _ _ -> if all isFlatState substates
                                 then c
                                 else error "flattening failed") c
    isFlatState InnerMostState{} = True
    isFlatState EndState{} = True
    isFlatState _ = False

flatten' :: (Eq l, Enum l, Num l, Ord l, Ord n, Show l) => UMLStateDiagram [n] l -> UMLStateDiagram [n] l
flatten'
 = maybe (error "not defined") distinctLabels
   . lift
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

lift :: (Eq l, Ord l, Enum l, Num l, Ord n, Eq n, Show l) => UMLStateDiagram [n] (Either l l) -> Maybe (UMLStateDiagram [n] (Either l l))
lift
  = fmap umlStateDiagram . unUML
    (\globalName rootNodes globalConnections globalStartState ->
      case find (\x -> isHierarchical x || isCombine x) rootNodes
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
      Just CombineDiagram { label = combineAddress
                          , substates = combineRegions }
        -> Just (
             StateDiagram
               { name = globalName
               , startState = [Left 4] -- temp; the rules are the same as for rewireCombineEntries and rewireForkInFlows
               , label = undefined
               , substates
                   = let
                     resolvedForks
                       = map (maybe (error "") fst . uncons)
                             (nub (map pointFrom
                                  (filter (\case
                                              Connection{ pointFrom = (Left pF):_
                                                        , pointTo = pT:_ }
                                                -> Left pF `elem` map label (filter isFork rootNodes) && pT == combineAddress
                                              _ -> False)
                                  globalConnections)))
                     resolvedJoins
                       = map (maybe (error "") fst . uncons) (nub (map pointTo
                        (filter (\case
                                    Connection{ pointFrom = pF:_
                                              , pointTo = (Left pT):_ }
                                      -> Left pT `elem`  map label (filter isJoin rootNodes) && pF == combineAddress
                                    _ -> False)
                        globalConnections)))
                     in
                     zipWith (\x y -> trimapW id
                                              (mapRight (const y))
                                              (mapRight (const y))
                              x)
                     (cartesianProductOf' combineAddress combineRegions) [1..]
                     ++
                     filter (\node
                                -> label node /= combineAddress &&
                                   label node `notElem` resolvedForks &&
                                   label node `notElem` resolvedJoins )
                     rootNodes
               , connections
                   = let r' = zip (map label
                              (cartesianProductOf' combineAddress combineRegions))
                              (map label
                              (fmap (\(x, y)
                                       -> trimapW id
                                                  (mapRight (const y))
                                                  (mapRight (const y))
                                          x)
                              (zip (cartesianProductOf' combineAddress combineRegions)
                                   [1..])
                              ))
                     in
                     mapNotRewired
                       (cartesianConnections'  -- forms a set of cartesian connections matching the cartesian nodes
                         combineAddress
                         combineRegions
                         r')
                     $
                     rewireJoinOutFlows  -- must be aware of multiple join inbound connections to fuse them
                       combineAddress
                       (map label $ filter isJoin rootNodes) r'
                     $
                     rewireForkInFlows -- must be aware of other connections and their target forks to fuse them
                       -- combineAddress
                       (map label $ filter isFork rootNodes)
                     $
                     rewireCombineEntries -- must know entry groups of transitions to complete them
                       combineAddress
                       (map ((:) combineAddress . (\n -> label n:startState n)) combineRegions)
                       r' globalConnections
               }
         )
      Just _
        -> error "we don't expect anything else than StateDiagram or Nothing here"
      Nothing
        -> Nothing
    )
  where
    isState InnerMostState{} = True
    isState CombineDiagram{} = True
    isState other = isHierarchical other
    isHierarchical StateDiagram{} = True
    isHierarchical _ = False
    isCombine CombineDiagram{} = True
    isCombine _ = False
    isFork Fork{} = True
    isFork _ = False
    isJoin Join{} = True
    isJoin _ = False

-- todo: adjust to [([[l]],Either l l)] -> [[l]] -> Either l l
--       or [[[Either l l]], Either l l] -> [[Either l l]] -> Either l l
translateTo :: (Show l, Eq l) => [(Either l [[l]], Either l l)] -> Either l [[l]] -> Either l l
translateTo cartesianRelation label
  = Data.Maybe.fromMaybe
    (error ("cant translate cartesian label: " ++ show label ++ " with cartesian relation: " ++ show cartesianRelation))
    (lookup label cartesianRelation)

-- todo: get rid of the trifunctor
cartesianProductOf' :: (Ord a1, Ord a2, Ord (z (Either a3 [[b]])), TrifunctorW StateDiagram z) => Either a1 r -> [StateDiagram [a2] (Either a1 a1) [z (Either a3 b)]]
 -> [StateDiagram [a2] (Either a1 [[a1]]) [z (Either a3 [[b]])]]
cartesianProductOf' combineAddress combineRegionNodes
    = case
        uncons $
             map ((fromList . (\case
                    StateDiagram { label = Left regionAddress
                                 , substates = nodesOfRegion }
                      -> fmap (\case
                                innerMost@InnerMostState{ label = Left label}
                                  -> innerMost {
                                       label
                                           -- todo: change to either-less variant, left side is never used for cartesian nodes anyway
                                         = Right [fromLeft' combineAddress : regionAddress : [label]]
                                     }
                                _ -> error "only orthogonal regions with plain InnerMostStates are supported"
                              )
                             nodesOfRegion
                    _ -> error "CombineDiagram node can't contain other node types than StateDiagram(s)"))
                   . trimapW id (mapRight (singleton . singleton)) (mapRight (singleton . singleton))
                 ) combineRegionNodes
      of
      Just (h,t) -> toList $ foldl (\x y ->
        fromList $
        concatMap (\case
                     ( (,) InnerMostState{ label = Right labelX
                                         , name = nameX
                                         , operations = operationsX }
                           InnerMostState{ label = Right labelY
                                         , name = nameY
                                         , operations = operationsY } )
                       -> [InnerMostState { label = Right (labelX ++ labelY)
                                          , name = nameX ++ nameY
                                          , operations = operationsX ++ operationsY }]
                     _ -> error "only orthogonal regions with plain InnerMostStates are supported")
        $ toList
        $ cartesianProduct x y) h t
      Nothing -> error "an empty CombineDiagram node, without any regions is ~not~ supported"

-- todo: get rid of the trifunctor
cartesianConnections' :: (Ord l, Ord a1, Ord (z (Either a2 [[b]])), TrifunctorW StateDiagram z, Show l) => Either l l -> [StateDiagram [a1] (Either l l) [z (Either a2 b)]]
 -> [(Either l [[l]], Either l l)] -> [Connection (Either l l)] -> [Connection (Either l l)]
cartesianConnections' combineAddress combineRegions cartesianRelation globalConnections
                           = concatMap concat
                             [ [ let
                                  cartesianTarget = [ case find (\c@Connection {}
                                                                   -> label == map fromLeft' (pointFrom c))
                                                           connectionGroup
                                                      of
                                                        Nothing -> (False, label)
                                                        Just c@Connection{} -> (True, map fromLeft' $ pointTo c)
                                                    | label <- fromRight' cartesianSource
                                                    ]
                                  in
                                  [ Connection { pointFrom = singleton $ translateTo cartesianRelation cartesianSource
                                               , pointTo = singleton $ translateTo cartesianRelation $ Right (map snd cartesianTarget)
                                               , transition = case uncons connectionGroup of
                                                                Just (x,_)
                                                                  -> transition x
                                                                _ -> error "can't be empty, therefore inner Forks are not supported"
                                               }
                                  | any fst cartesianTarget
                                  ]
                               | connectionGroup <- groupBy (\x y -> transition x == transition y) $
                                                    sortBy (\x y -> compare (transition x) (transition y)) $
                                                    filter (\case
                                                               Connection { pointFrom = (y:_)
                                                                          , pointTo = (x:_) }
                                                                 -> fromLeft' y == fromLeft' combineAddress &&
                                                                    fromLeft' x == fromLeft' combineAddress
                                                               _ -> False )
                                                    (fmap (fmap (mapRight (singleton . singleton))) globalConnections)
                               ]
                             | cartesianSource <- map label (cartesianProductOf' combineAddress combineRegions)
                             ]
                             ++
                             filter
                             (\case
                                Connection { pointFrom = pF:_
                                           , pointTo = pT:_ }
                                  -> pF /= combineAddress &&
                                     pT /= combineAddress
                                _ -> False
                             ) globalConnections

rewireJoinOutFlows :: (Eq l, Ord l, Show l) => Either l l -> [Either l l] -> [(Either l [[l]], Either l l)] -> [Connection (Either l l)] -> [Connection (Either l l)]
rewireJoinOutFlows combineAddress joinAddresses cartesianRelation connections
  = concatMap (\case
                 -- rewire triggered outgoing connection from join by taking its immediate inbound flows; (a,b) -> (b,c) => (a,c)
                 connection@Connection { pointFrom = [pF]
                                       , pointTo = pT }
                   -> if pF `elem` joinAddresses &&
                         any (\case
                                Connection { pointTo = [pT']
                                           , pointFrom = pf':_}
                                  -> pT' == pF && pf' == combineAddress
                                _ -> False)
                             connections
                      then [ Connection { pointTo = pT
                                        , transition
                                            = maybe (error "no outbound join transition found or missing label")
                                              transition
                                              (find (\case
                                                          Connection { pointFrom = pf:_
                                                                     , pointTo = [pt] }
                                                            -> pf == combineAddress &&
                                                               pt == pF
                                                          _ -> False)
                                                 connections)
                                        , pointFrom
                                            = singleton $
                                              translateTo cartesianRelation $
                                              Right $
                                              map (map fromLeft' . pointFrom) (filter (\case
                                                          Connection { pointFrom = pf:_
                                                                     , pointTo = [pt] }
                                                            -> pf == combineAddress &&
                                                               pt == pF
                                                          _ -> False)
                                                 connections)
                                          }
                           ]
                      else [connection]
                -- remove inbound join connection; (a,b) -> (b,c) => drop (a,b)
                 connection@Connection { pointFrom = pF:_
                                       , pointTo = [pT] }
                   -> if pF == combineAddress &&
                         pT `elem` joinAddresses
                      then []
                      else singleton
                           connection
                 -- do not alter other connections
                 connection -> singleton connection
              ) connections

rewireForkInFlows :: (Eq l, Ord l, Show l) => [Either l l] -> [Connection (Either l l)] -> [Connection (Either l l)]
rewireForkInFlows forkAddresses connections
  = concatMap (\case
                 -- rewire triggered outgoing connection from fork to its immediate source; (a,b) -> (b,c) => (a,c)
                 connection@Connection { pointFrom = [pF]
                                       , pointTo = [Right _] }
                   -> if pF `elem` forkAddresses
                      then singleton $
                           connection { pointFrom
                                          = maybe (error "fork is not triggered by any incoming connection")
                                                  pointFrom
                                                  (find (\case
                                                           Connection { pointTo = [pT'] }
                                                             -> pT' == pF
                                                           _ -> False)
                                                        connections)
                                      , transition
                                          = maybe (error "fork is not triggered by any incoming connection or without a literal")
                                                  transition
                                                  (find (\case
                                                           Connection { pointTo = [pT']
                                                                      , transition = name }
                                                             -> pT' == pF && not (null name)
                                                           _ -> False)
                                                        connections)
                                      }
                      else [connection]
                -- remove flow into triggered fork; (a,b) -> (b,c) => drop (a,b)
                 connection@Connection { pointTo = [pT] }
                   -> if (pT `elem` forkAddresses) &&
                         any (\case
                                  Connection { pointFrom = [pF']
                                             , pointTo = [Right _] }
                                    -> (pF' == pT)
                                  _ -> False)
                          connections
                      then []
                      else singleton
                           connection
                 -- do not alter other connections
                 connection -> singleton connection
              ) connections

rewireCombineEntries :: (Ord l, Show l, Num l, Enum l) => Either l l -> [[Either l l]] -> [(Either l [[l]], Either l l)] -> [Connection (Either l l)] -> [Connection (Either l l)]
rewireCombineEntries combineAddress combineInitialStates cartesianRelation connections
  = concatMap (concatMap (toCartesianTarget combineAddress combineInitialStates cartesianRelation) .
    (groupBy (\x y -> transition x == transition y) .  -- transition groups
    sortBy (\x y -> compare (transition x) (transition y))))
    ((groupBy (\x y -> pointFrom x == pointFrom y) .  -- source groups
    sortBy (\x y -> compare (pointFrom x) (pointFrom y)))
    connections)
  where
  -- single implicit combine node activation
  toCartesianTarget combineAddress' combineInitialStates' cartesianRelation'
                    [Connection { pointTo = [pT]
                                , pointFrom = pF@(pf:_)
                                , transition = name } ]
    | pT == combineAddress' &&
      pf /= combineAddress'
    = singleton $
      Connection { pointFrom = map (Left . fromLeft') pF
                 , pointTo
                     = singleton $
                       translateTo cartesianRelation' $
                       Right (map (map fromLeft') combineInitialStates')
                 , transition = name }
  -- fork activation of combine node (partial, or complete)
  toCartesianTarget combineAddress' combineInitialStates' cartesianRelation'
                    connectionGroup@(Connection { pointTo = pT:_
                                                , pointFrom = pF@(pf:_)
                                                , transition = name }:_)
    | pT == combineAddress' &&
      pf /= combineAddress'
    = singleton $
      Connection { pointFrom = map (Left . fromLeft') pF
                 , pointTo
                     = singleton $
                       translateTo cartesianRelation' $
                       Right $
                       -- override initial state with forked entries
                       map (map fromLeft' . (\case
                              initialState@(_:region:_)
                                -> case find (\case
                                                Connection { pointTo = (_:region':_) }
                                                  -> region == region'
                                                _ -> False )
                                        connectionGroup
                                   of
                                   (Just (Connection { pointTo = overrideInitial }))
                                     -> overrideInitial
                                   Nothing -> initialState
                              _ -> error "malformed initial state"
                            )) combineInitialStates'
                , transition = name }
    -- leave connections not causing an activation of the combine node unaltered
    | otherwise = connectionGroup
  toCartesianTarget _ _ _ connections' = connections'

mapNotRewired :: ([Connection (Either a b)] -> [Connection (Either a b)]) -> [Connection (Either a b)] -> [Connection (Either a b)]
mapNotRewired f c
  = f (filter notRewired c) ++ filter rewired c
  where
  notRewired Connection { pointFrom = (Left _:_)
                        , pointTo = (Left _:_) }
    = True
  notRewired _ = False
  rewired Connection { pointTo = [Right _] }
    = True
  rewired Connection { pointFrom = [Right _] }
    = True
  rewired _ = False

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
  = [ c { pointFrom
            = mapHeadTail (`matchToRelation` r) fromLeft' (pointFrom c)
        , pointTo
            = mapHeadTail (`matchToRelation` r) fromLeft' (pointTo c)
        } | c <- connections ]
