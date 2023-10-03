{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE LambdaCase                #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Flatten (
  flatten
 ,distinctLabels'
 ,updateVertex
 ,vertexPath
 ,allVertexAddresses
 ,vertexByAddress) where

import Datatype (UMLStateDiagram
                ,umlStateDiagram
                ,unUML
                ,StateDiagram(..)
                ,globalise
                ,Connection(..)
                ,rename, HistoryType (..)
                )
import Datatype.ClassInstances ()
import Data.Either.Extra (fromLeft', fromRight')
import Data.List (find
                 ,unfoldr)

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

{- return the substates of vertices that have their own children, or [] if not -}
substates' :: StateDiagram n b [Connection b] -> [StateDiagram n b [Connection b]]
substates' (StateDiagram a _ _ _ _) = a
substates' (CombineDiagram a _) = a
substates' _ = []

leftLabelConnection :: Connection a -> Connection (Either a b)
leftLabelConnection
  Connection{ pointFrom = pf
            , pointTo = pt
            , transition = t }
    = Connection { pointFrom = map Left pf
                 , pointTo = map Left pt
                 , transition = t }

{- i should really figure out how to do proper functional traversals -}
leftLabelNodes :: StateDiagram n a1 [Connection a2] -> StateDiagram n (Either a1 b1) [Connection (Either a2 b2)]
leftLabelNodes
  StateDiagram { label = sdLabel
               , substates = sdSubstates
               , startState = sdStartState
               , connections = sdConnections
               , name = sdName }
    = StateDiagram { label = Left sdLabel
                   , substates = map leftLabelNodes sdSubstates
                   , startState = map Left sdStartState
                   , connections = map leftLabelConnection sdConnections
                   , name = sdName }
leftLabelNodes
  CombineDiagram { label = cdLabel
                 , substates = cdSubstates }
    = CombineDiagram { label = Left cdLabel
                     , substates = map leftLabelNodes cdSubstates }
leftLabelNodes
  EndState { label = esLabel }
    = EndState { label = Left esLabel }
leftLabelNodes
  ForkOrJoin { label = fjLabel }
    = ForkOrJoin { label = Left fjLabel }
leftLabelNodes
  History { label = hLabel
          , historyType = hType }
    = History { label = Left hLabel
              , historyType = hType }
leftLabelNodes
  InnerMostState { label = imsLabel
                 , name = imsName
                 , operations = imsOperations }
    = InnerMostState { label = Left imsLabel
                     , name = imsName
                     , operations = imsOperations }

fromRightLabeledConnection :: Connection (Either l l) -> Connection l
fromRightLabeledConnection
  = \case
      Connection { pointFrom = pf
                 , pointTo = pt
                 , transition = t }
        -> Connection { pointFrom = map fromLeft' pf {- TODO: fix that the naming aligns, or better dont unwrap from Either too early -}
                      , pointTo = map fromLeft' pt
                      , transition = t }

fromRightLabeledNodes :: StateDiagram n (Either l l) [Connection (Either l l)] -> StateDiagram n l [Connection l]
fromRightLabeledNodes
  = \case
      StateDiagram { label = sdLabel
                   , substates = sdSubstates
                   , startState = sdStartState
                   , connections = sdConnections
                   , name = sdName }
        -> StateDiagram { label = fromRight' sdLabel
                        , substates = map fromRightLabeledNodes sdSubstates
                        , startState = map fromRight' sdStartState
                        , connections = map fromRightLabeledConnection sdConnections
                        , name = sdName }
      CombineDiagram { label = cdLabel
                     , substates = cdSubstates }
        -> CombineDiagram { label = fromRight' cdLabel
                          , substates = map fromRightLabeledNodes cdSubstates }
      EndState { label = esLabel }
        -> EndState { label = fromRight' esLabel }
      ForkOrJoin { label = fjLabel }
        -> ForkOrJoin { label = fromRight' fjLabel }
      History { label = hLabel
              , historyType = hType }
        -> History { label = fromRight' hLabel
                   , historyType = hType }
      InnerMostState { label = imsLabel
                     , name = imsName
                     , operations = imsOperations }
        -> InnerMostState { label = fromRight' imsLabel
                          , name = imsName
                          , operations = imsOperations }

{- return the vertex of a chart referenced by the given address -}

{- return the vertex of a chart referenced by the given address -}
vertexByAddress ::(Eq b, Show b, Show n) => [b] -> StateDiagram n b [Connection b] -> StateDiagram n b [Connection b]
vertexByAddress [] stateChart = stateChart
vertexByAddress address stateChart
  = fst (vertexPath address stateChart)

{- return the path of nodes traversed before reaching the specified vertex by its address in a chart -}
nodePathToVertex :: (Eq b, Show b, Show n) => [b] -> StateDiagram n b [Connection b] -> [StateDiagram n b [Connection b]]
nodePathToVertex address stateChart
  = snd (vertexPath address stateChart)

{- auxiliary function, returns the path to and the vertex reached by a given address in a chart -}
vertexPath :: (Eq b, Show b, Show n) => [b] -> StateDiagram n b [Connection b] -> (StateDiagram n b [Connection b],[StateDiagram n b [Connection b]])
vertexPath globalVertexAddress stateChart
  = let
    vertexPath'
      = unfoldr
        (\(vertexAddress,stateChart')
          -> if null vertexAddress
             then Nothing
             else case find ((head vertexAddress ==) . label) (substates' stateChart') of
                    Nothing -> error (("bad traversal, vertex by label not found in search for " ++ show (head vertexAddress)) ++ " being at: " ++ show stateChart')
                    Just vertex
                      -> Just (vertex,(tail vertexAddress,vertex)))
        (globalVertexAddress,stateChart)
    in
    (last vertexPath', stateChart : init vertexPath')

{- updates a chart, replacing the vertex reached by an address with a node provided carrying the *same* label -}
updateVertex :: (Eq b, Show b, Show n) => [b] -> StateDiagram n b [Connection b] -> StateDiagram n b [Connection b] -> StateDiagram n b [Connection b]
updateVertex vertexAddress newVertex stateChart
  = if label newVertex /= last vertexAddress
    then error "vertex updated must carry the *same* label as the vertex it replaces; or you might want to use replaceVertex instead"
    else
    foldl replaceVertexL newVertex (nodePathToVertex vertexAddress stateChart)

{- replaces a vertex within a chart, specified by its address with a provided node -}
replaceVertex :: (Eq b, Show b, Show n) => [b] -> StateDiagram n b [Connection b] -> StateDiagram n b [Connection b] -> StateDiagram n b [Connection b]
replaceVertex [] newVertex _ = newVertex
replaceVertex [x] newVertex stateChart
  = if any (\z -> x == label z) (substates' stateChart)
    then
    case stateChart of
      s@StateDiagram{}
        -> s { substates = newVertex : filter (\z -> x /= label z) (substates' s) }
      c@CombineDiagram{}
        -> c { substates = newVertex : filter (\z -> x /= label z) (substates' c) }
      _ -> error "parent vertex can't be updated because it is not able to carry children"
    else error "the address given, points towards a vertex that doesn't exist"
replaceVertex vertexAddress newVertex stateChart
  = let
    updatedVertex = replaceVertex [last vertexAddress] newVertex (last (nodePathToVertex vertexAddress stateChart))
    in
    foldl replaceVertexL updatedVertex (init (nodePathToVertex vertexAddress stateChart))

{- auxiliary function for folding together vertex paths into one root node, to store changes made -}
replaceVertexL :: (Eq a, Show a) => StateDiagram n a [Connection a] -> StateDiagram n a [Connection a] -> StateDiagram n a [Connection a]
replaceVertexL x y
  = case y of
      s@StateDiagram{}
        -> s { substates = x : filter (\z -> label x /= label z) (substates' s) }
      c@CombineDiagram{}
        -> c { substates = x : filter (\z -> label x /= label z) (substates' c) }
      _ -> error "impossible to traverse updwards"

{- polymorphic variant of checkers helper function,
   TODO: it must contain [] as root address as well -}
allVertexAddresses :: (Show b) => StateDiagram n b [Connection b] -> [[b]]
allVertexAddresses stateChart
  = [] :
    map (\x -> [label x]) (substates' stateChart)
    ++ concatMap allVertexAddresses' (substates' stateChart)
    where
    allVertexAddresses' stateChart'
      = map (\x -> label stateChart' : [label x]) (substates' stateChart')
        ++ concatMap allVertexAddresses' (substates' stateChart')

{- enumerate all vertices in the chart with unique labels -}
distinctLabels' :: (Eq a, Enum a, Show a, Num a, Show n) => StateDiagram n a [Connection a] -> StateDiagram n a [Connection a]
distinctLabels' stateChart
  = let
    rewireStartStates relabeledChart
      = foldl (\stateChart' (addr,startState')
                 -> replaceVertex (lookupByAddr addr)
                    (case vertexByAddress (lookupByAddr addr) stateChart' of
                       s@StateDiagram{}
                         -> s { startState
                                  = case lookup (addr ++ startState') -- awkward inlining issue? type mismatch when deferring to lookupByAddr
                                         (zip (allVertexAddresses stateChart)
                                              (allVertexAddresses relabeledChart))
                                    of
                                    Just addr'' -> addr''
                                    Nothing -> error "bad resolution" }
                       _ -> error "invalid node" ) stateChart'
                ) relabeledChart (reverse globalizedStartStates)
        where
        lookupByAddr addr
          = (\case
               Just addr' -> addr'
               Nothing -> error "bad resolution")
            $ lookup addr (zip (allVertexAddresses stateChart)
                               (allVertexAddresses relabeledChart))
        globalizedStartStates
          = concatMap (\addr
                         -> case vertexByAddress addr stateChart of
                              StateDiagram { startState } -> [(addr,startState)]
                              _ -> []
                      ) (allVertexAddresses stateChart)
    rewireGlobalizedConnections relabeledChart
      = case relabeledChart of
          s@StateDiagram{ connections = rootConnections }
            -> s { connections = rewiredConnections rootConnections }
          _ -> error "only StateDiagram constructor can carry connections"
        where
        rewiredConnections
          = map (\case
                   c@Connection{ pointFrom = pF
                               , pointTo = pT }
                     -> c { pointFrom = case lookup pF (zip (allVertexAddresses stateChart) (allVertexAddresses relabeledChart)) of
                                           Just x -> x
                                           Nothing -> error "bad resolution"
                           , pointTo = case lookup pT (zip (allVertexAddresses stateChart) (allVertexAddresses relabeledChart)) of
                                         Just x -> x
                                         Nothing -> error "bad resolution" })
    relabelVertices labelSet stateDiagram {- use left, right to tag whether a hit vertex is already relabeled or not -}
      = fromRightLabeledNodes $ {- it might be beneficial to keep LR until rewiring operations are done (to know that its not finished yet) -}
        foldl (\stateChart' (addr,l)
                 -> replaceVertex addr ((vertexByAddress addr stateChart') { label = l}) stateChart')
        (leftLabelNodes stateDiagram) (zip (map (map Left) (reverse $ allVertexAddresses stateDiagram)) (map Right labelSet))
    globalizeConnections
      = (\case
           s@StateDiagram {}
            -> s { connections
                     = concatMap
                       (\addr -> case vertexByAddress addr stateChart of
                                   StateDiagram { connections }
                                     -> map (\case
                                               c@Connection { pointFrom, pointTo }
                                                 -> c { pointFrom = addr ++ pointFrom
                                                      , pointTo = addr ++ pointTo }
                                            ) connections
                                   _ -> []
                        ) (reverse $ allVertexAddresses stateChart) }
           _ -> error "connections can only be globalized into a StateDiagram node"
        ) (foldl (\stateChart' addr -- this part strips all connections from their nodes, so that the upper section can create a single occurence of them in the root vertex
                     -> (case vertexByAddress addr stateChart' of
                           s@StateDiagram{}
                             -> replaceVertex addr (s { connections = [] }) stateChart'
                           e -> replaceVertex addr e stateChart' -- yeah, we have got to do this one, to persist changes on nodes that might be able to; but do not carry connections in the chain upwards to the root and are intermediates
                         )
                  ) stateChart (reverse $ allVertexAddresses stateChart))
    in
    rewireStartStates $
    rewireGlobalizedConnections $
    relabelVertices [1..]
    globalizeConnections -- Int independent variant
    -- TODO: inline oldToNew addresses relation

