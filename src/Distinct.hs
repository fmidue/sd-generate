{-# LANGUAGE NamedFieldPuns               #-}
{-# LANGUAGE LambdaCase                   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fromMaybe"          #-}

module Distinct (distinctLabels) where

import Data.Either.Extra (fromRight')
import Data.List (find
                 ,unfoldr)
import Datatype (StateDiagram(..)
                ,Connection(..))

{- return the substates of vertices that have their own children, or [] if not -}
substates' :: StateDiagram n b [Connection b] -> [StateDiagram n b [Connection b]]
substates' (StateDiagram a _ _ _ _) = a
substates' (CombineDiagram a _) = a
substates' _ = []

leftLabelConnection :: (Eq a) => Connection a -> Connection (Either a a)
leftLabelConnection
  Connection{ pointFrom = pf
            , pointTo = pt
            , transition = t }
    = Connection { pointFrom = map Left pf
                 , pointTo = map Left pt
                 , transition = t }

leftLabelNodes :: (Eq l, Eq n) => StateDiagram n l [Connection l] -> StateDiagram n (Either l l) [Connection (Either l l)]
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

fromRightLabeledConnection :: (Eq l) => Connection (Either l l) -> Connection l
fromRightLabeledConnection
  = \case
      Connection { pointFrom = pf
                 , pointTo = pt
                 , transition = t }
        -> Connection { pointFrom = map fromRight' pf
                      , pointTo = map fromRight' pt
                      , transition = t }

fromRightLabeledNodes :: (Eq l, Eq n) => StateDiagram n (Either l l) [Connection (Either l l)] -> StateDiagram n l [Connection l]
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
    where
    replaceVertexL x y
      = case y of
          s@StateDiagram{}
            -> s { substates = x : filter (\z -> label x /= label z) (substates' s) }
          c@CombineDiagram{}
            -> c { substates = x : filter (\z -> label x /= label z) (substates' c) }
          _ -> error "impossible to traverse updwards"

{- polymorphic variant of checkers helper function -}
allVertexAddresses :: (Show b, Eq b) => StateDiagram n b [Connection b] -> [[b]]
allVertexAddresses stateChart
  = [] :
    map (\x -> [label x]) (substates' stateChart)
    ++ concatMap allVertexAddresses' (substates' stateChart)
    where
    allVertexAddresses' stateChart'
      = map (\x -> label stateChart' : [label x]) (substates' stateChart')
        ++ concatMap allVertexAddresses' (substates' stateChart')

{- auxiliary function looking up a the vertex address in a relabeled chart for a vertex address of a non-relabeled chart -}
lookupByAddr :: (Show l, Eq l) => [Either l l] -> StateDiagram n (Either l l) [Connection (Either l l)] -> StateDiagram n l [Connection l] -> [Either l l]
lookupByAddr addr relabeledChart stateChart {- TODO; maybe swap the args for readability -}
          = maybe (error "lookup failed") id
            $ lookup addr (zip (map (map Left) (allVertexAddresses stateChart))
                               (allVertexAddresses relabeledChart))

{- enumerate all vertices in the chart with unique labels -}
distinctLabels :: (Eq n, Num l, Enum l, Show n, Show l, Eq l) => StateDiagram n l [Connection l] -> StateDiagram n l [Connection l]
distinctLabels stateChart
  = let
    rewireStartStates relabeledChart
      = foldl (\stateChart' (addr,startState')
                 -> replaceVertex (lookupByAddr addr relabeledChart stateChart)
                    (case vertexByAddress (lookupByAddr addr relabeledChart stateChart) stateChart' of
                       s@StateDiagram{}
                         -> s { startState
                                  = drop (length addr) $ lookupByAddr (addr ++ startState') relabeledChart stateChart
                              }
                       _ -> error "invalid node" ) stateChart'
                ) relabeledChart (reverse globalizedStartStates)
        where
        globalizedStartStates
          = concatMap (\addr
                         -> case vertexByAddress addr stateChart of
                              StateDiagram { startState } -> [(map Left addr,map Left startState)]
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
                     -> c { pointFrom = lookupByAddr pF relabeledChart stateChart
                          , pointTo = lookupByAddr pT relabeledChart stateChart })
    relabelVertices labelSet stateDiagram {- use left, right to tag whether a hit vertex is already relabeled or not -}
      = foldl (\stateChart' (addr,l)
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
    fromRightLabeledNodes $ -- now, with all nodes Right after relabeling, this call should not fail, unless something was forgotten and addresses somehow stayed Left
    rewireStartStates $
    rewireGlobalizedConnections $
    relabelVertices [1..] -- here the state chart is labeled Left, so that the relabeling can be done with Right labels
    globalizeConnections -- UMLStateChart independent variant
