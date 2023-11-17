{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}



module Modelling.StateDiagram.Flatten(flatten
                                     ,flatten'
                                     ,liftCD) where

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
import Data.List ( find, singleton, uncons, groupBy, (\\) )
import Data.Bifunctor (bimap)
import Control.Lens (over
                    ,traverseOf)
import Data.Set (fromList, toList, cartesianProduct)
import Data.List.Extra (sortBy)


{- convenience function to access n l a in one swipe
   it is to be removed by fmap and bimap in the future
   because n/a is rarely needed to be accessed after all
   (only rename needs it)
-}
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


liftCD :: UMLStateDiagram [String] (Either Int Int) -> Maybe (UMLStateDiagram [String] (Either Int [[Int]]))
liftCD
  = fmap umlStateDiagram . unUML
    (\globalName rootNodes globalConnections _ {-globalStartState-}
      -> case find isCombine rootNodes
          of
          Just CombineDiagram { label = combineAddress
                              , substates = combineRegions
                              }
            -> Just (
                 StateDiagram
                   { name = globalName
                   , startState = []
                   , label = undefined
                   , substates
                       = cartesianProductOf combineRegions
                         ++
                         -- bimap should be enough
                         map (trimapW id (mapRight (singleton . singleton)) (mapRight (singleton . singleton)))
                         (filter ((combineAddress /=) . label) rootNodes)
                   , connections
                       = let
                         cartesianConnections -- maybe just hand back c instead of singleton c, makes one less wrap
                           = concatMap concat
                             [ [ let  -- i should have used maps instead... this is anything but modular at this stage
                                  {- given a node created as part of the computed
                                     cartesian product of the combine node's regions,
                                     we match every group of transitions determined by
                                     their literal to bind against their updated cartesian target -}
                                  cartesianTarget = [ case find (\c@Connection {}
                                                                   -> label == tail (map fromLeft' $ pointFrom c))
                                                           connectionGroup
                                                      of
                                                        Nothing -> (False, label)
                                                        Just c@Connection{} -> (True, tail $ map fromLeft' $ pointTo c)
                                                    | label <- fromRight' cartesianSource ]
                                  in
                                  {- assemble the the updated connection, by binding it against its
                                     cartesian target (identified through the Right label)
                                  -}
                                  [ Connection { pointFrom = [cartesianSource]
                                               , pointTo = [Right (map snd cartesianTarget)]
                                               , transition = case uncons connectionGroup of
                                                                Just (x,_)
                                                                  -> transition x
                                                                _ -> error "can't be empty, therefore inner Forks are not supported"
                                               } | any fst cartesianTarget ]
                                                   {- connection groups might not link a given
                                                      cartesian source with a cartesian target,
                                                      in that case we do not form them -}
                                  {- group all connections spanned between nodes of orthogonal regions
                                     belonging to the combine diagram by their literals into sets -}
                                | connectionGroup <- groupBy (\x y -> transition x == transition y) $
                                                     sortBy (\x y -> compare (transition x) (transition y)) $
                                                     {- we only want to rewire connections spanned between cartesian nodes
                                                        and nothing else, anything happening beyond the combine node
                                                        is therefore not of interest here -}
                                                     filter (\case
                                                                Connection { pointFrom = (y:_)
                                                                           , pointTo = (x:_) }
                                                                  -> fromLeft' y == fromLeft' combineAddress &&
                                                                     fromLeft' x == fromLeft' combineAddress
                                                                _ -> False )
                                                     (fmap (fmap (mapRight (singleton . singleton))) globalConnections)
                                ]
                                {- connections are meant to be spanned between cartesian nodes that
                                   are the outcome of the cartesian product construction, with taking
                                   the orthogonal regions of the combine diagram and crossing them.
                                   Therefore, we gather the labels of that construction, since old node
                                   values became part of them.
                                   Labels in of cartesian nodes are always Right tagged, somewhat similar to the lift SD approach above. -}
                              | cartesianSource <- map label (cartesianProductOf combineRegions)
                              ]
                         rootNodes' = fmap (trimapW id (mapRight (singleton . singleton)) (mapRight (singleton . singleton))) rootNodes
                         globalConnections' = fmap (fmap (mapRight (singleton . singleton))) globalConnections
                         joinedConnections
                           = filter (\c -> maybe (error "connection target undefined")
                                                 (\(h,_) -> h `elem` map label (filter isJoin rootNodes'))
                                                 (uncons $ pointTo c)
                                             &&
                                             maybe (error "connection source undefined")
                                                   (\(h,_) -> fromLeft' h == fromLeft' combineAddress)
                                                   (uncons $ pointFrom c))
                            globalConnections'
                         forkedConnections
                           = filter (\c -> maybe (error "connection source undefined")
                                                 (\(h,_) -> h `elem` map label (filter isFork rootNodes'))
                                                 (uncons $ pointFrom c)
                                             &&
                                             maybe (error "connection target undefined")
                                                   (\(h,_) -> fromLeft' h == fromLeft' combineAddress)
                                                   (uncons $ pointTo c))
                            globalConnections'
                         forkTriggerConnections
                           = filter (\c -> maybe (error "connection source undefined")
                                                 (\(h,_) -> h `elem` map label (filter isFork rootNodes'))
                                                 (uncons $ pointTo c)
                                           &&
                                           any (\Connection{ pointFrom = pF
                                                            , pointTo = pT }
                                                  -> pointTo c == pF &&
                                                     maybe (error "connection target undefined")
                                                           (\(h,_) -> fromLeft' h == fromLeft' combineAddress)
                                                           (uncons pT)
                                               ) forkedConnections
                                    )
                            globalConnections'
                         joinMergedConnections
                           = filter (\c -> maybe (error "connection source undefined")
                                                 (\(h,_) -> h `elem` map label (filter isJoin rootNodes'))
                                                 (uncons $ pointFrom c)
                                           &&
                                           any (\Connection{ pointFrom = pF
                                                           , pointTo = pT }
                                                  -> pointFrom c == pT &&
                                                     maybe (error "connection target undefined")
                                                           (\(h,_) -> fromLeft' h == fromLeft' combineAddress)
                                                           (uncons pF)
                                               ) joinedConnections
                                    )
                            globalConnections'
                         inertConnections
                           = filter (\case
                                    {- connections that reach into, or are part of inter connections
                                       between nodes of the combine diagram are dismissed as they do change
                                       the rest is Left labelled and remains 'mostly' valid.
                                       The exception is formed by Join and Fork nodes, these are structurally valid
                                       but semantically not exactly correct and need to be removed at a later stage. -}
                                    Connection { pointFrom = (y:_)
                                               , pointTo = (x:_) }
                                      -> fromLeft' y /= fromLeft' combineAddress &&
                                         fromLeft' x /= fromLeft' combineAddress
                                    _ -> False )  ((((globalConnections' \\ joinedConnections) \\ forkedConnections) \\ joinMergedConnections) \\ forkTriggerConnections)
                        --convertedForks
                        --  = map pointTo forkTriggerConnections
                        --convertedJoins
                        --  = map pointFrom joinMergedConnections
                        --semiExplicitCDEntries
                        --  = filter (\c -> maybe (error "connection source undefined")
                        --                        (\(h,_) -> h `notElem` map label (filter isFork rootNodes'))
                        --                        (uncons $ pointFrom c)
                        --                  &&
                        --                  maybe (error "connection source undefined")
                        --                        (\(h,_) -> fromLeft' h == fromLeft' combineAddress)
                        --                        (uncons $ pointTo c)
                        --           )
                        --  globalConnections'
                         in
                         cartesianConnections
                         ++ inertConnections
                   }
                )
          Just _
            -> error "we don't expect anything else than StateDiagram or Nothing here"
          Nothing
            -> Nothing
    )
  where
  isCombine CombineDiagram{} = True
  isCombine _ = False
  isFork Fork{} = True
  isFork _ = False
  isJoin Join{} = True
  isJoin _ = False
  {- given a combine diagram, this function returns a cartesian
     product construction of its nodes.
     for that purpose, all previously Left tagged nodes will be converted to carry
     Right labels when "glued/crossed" against nodes of other regions.
     only InnerMostState nodes are supported as of now.
     when crossing two nodes, they will remember their region, aka. StateDiagram node
     that was their immediate parent.

     CombineDiagram(L5) ----> StateDiagram(L1) -----> InnerMost(L1)"A"
                         |
                         |---> StateDiagram(L2) ------> InnerMost(L1)"B"

    this sketchy asci schematic shall visualize the outcome;

                            InnerMost( R [1,1],[2,1] )"A","B"

    remembering their immediate parent allows to distinguish two nodes from each other
    that might carry the same numeric label in separate regions.
  -}
  cartesianProductOf combineRegionNodes
    = case
        uncons $
             map ((fromList . (\case
                    StateDiagram { label = Left regionAddress
                                 , substates = nodesOfRegion }
                      -> fmap (\case
                                innerMost@InnerMostState{ label = Left label}
                                  -> innerMost {
                                       label
                                         = Right [regionAddress : [label]]
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
                                {- in theory it could be possible to define rules for (SD x InnerMost) and vice versa as well -}
        $ toList
        $ cartesianProduct x y) h t
      Nothing -> error "an empty CombineDiagram node, without any regions is ~not~ supported"


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

matchConnectionToRelation :: (Eq l) => [Connection (Either l l)] -> [(Either l l, l)] -> [Connection l]
matchConnectionToRelation connections r
  = [ c { pointFrom
            = mapHeadTail (`matchToRelation` r) fromLeft' (pointFrom c)
        , pointTo
            = mapHeadTail (`matchToRelation` r) fromLeft' (pointTo c)
        } | c <- connections ]
