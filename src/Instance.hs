{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}
module Instance (
  parseInstance,
  ) where

import qualified Data.Map               as M (fromAscList, elems, lookup, empty, insert, fromList)
import qualified Data.Set               as S (unions, mapMonotonic, union, toAscList, partition, map, filter, member)

import Datatype (
  localise,
  UMLStateDiagram,
  StateDiagram (..),
  Connection (Connection),
  HistoryType (..),
  )

import Control.Monad                    (join)
import Control.Monad.Error.Class        (MonadError (throwError))
import Data.Bifunctor                   (Bifunctor (first))
import Data.List                        (partition)
import Data.List.Extra                  (nubOrd)
import Data.Map                         (Map)
import Data.Maybe (
  fromMaybe,
  fromJust,
  maybeToList,
  catMaybes,
  )
import Data.Set                         (Set)
import Data.String                      (IsString (fromString))
import Data.Tree                        (Forest, Tree(..))
import Data.Tuple.Extra                 (uncurry3)
import Language.Alloy.Call (
  getDoubleAs,
  getSingleAs,
  lookupSig,
  scoped,
  AlloyInstance,
  )

newtype ComponentName = ComponentName String
  deriving (Eq, Ord, Read, Show)
newtype TriggerName = TriggerName String
  deriving (Eq, Ord, Read, Show)

newtype RegionState = RegionState String
  deriving (Eq, Ord, Read, Show)
newtype State = State String
  deriving (Eq, Ord, Read, Show)
newtype CompositeState = CompositeState String
  deriving (Eq, Ord, Read, Show)
newtype ForkJoinNode = ForkJoinNode String
  deriving (Eq, Ord, Read, Show)
newtype EndNode = EndNode String
  deriving (Eq, Ord, Read, Show)
newtype StartNode = StartNode String
  deriving (Eq, Ord, Read, Show)
data HistoryNode = HistoryNode HistoryType String
  deriving (Eq, Ord, Show)

newtype Trigger = Trigger String
  deriving (Eq, Ord, Read, Show)

data Node =
  CNode CompositeState
  | ENode EndNode
  | FJNode ForkJoinNode
  | HNode HistoryNode
  | RNode RegionState
  | SNode State
  | StNode StartNode
  deriving (Eq, Ord, Show)

data Nodes = Nodes {
  cNodes  :: Set CompositeState,
  eNodes  :: Set EndNode,
  fjNodes :: Set ForkJoinNode,
  hNodes  :: Set HistoryNode,
  rNodes  :: Set RegionState,
  sNodes  :: Set State,
  stNodes :: Set StartNode
  } deriving Show

toSet :: Nodes -> Set Node
toSet ns = S.unions [
  CNode `S.mapMonotonic` cNodes ns,
  ENode `S.mapMonotonic` eNodes ns,
  FJNode `S.mapMonotonic` fjNodes ns,
  HNode `S.mapMonotonic` hNodes ns,
  RNode `S.mapMonotonic` rNodes ns,
  SNode `S.mapMonotonic` sNodes ns,
  StNode `S.mapMonotonic` stNodes ns
  ]

parseInstance
  :: (MonadError s m, IsString s)
  => String
  -> AlloyInstance
  -> m UMLStateDiagram
parseInstance scope insta = do
  states <- getAs "NormalStates" State
  composites <- S.union
    <$> getAs "Regions" CompositeState
    <*> getAs "HierarchicalStates" CompositeState
  regions <- getAs "RegionsStates" RegionState
  forkJoins <- S.union
    <$> getAs "ForkNodes" ForkJoinNode
    <*> getAs "JoinNodes" ForkJoinNode
  ends <- getAs "EndNodes" EndNode
  histories <- S.union
    <$> getAs "ShallowHistoryNodes" (HistoryNode Shallow)
    <*> getAs "DeepHistoryNodes" (HistoryNode Deep)
  stnodes <- getAs "StartNodes" StartNode
  let nodes = Nodes composites ends forkJoins histories regions states stnodes
  compositeContains <- fmap (S.mapMonotonic $ first CNode) $ S.union
    <$> getContains scope insta nodes "Regions" CompositeState
    <*> getContains scope insta nodes "HierarchicalStates" CompositeState
  regionContains <- S.mapMonotonic (first RNode)
    <$> getContains scope insta nodes "RegionsStates" RegionState
  componentNames <- fmap (M.fromAscList . S.toAscList) . S.union
    <$> getNames scope insta nodes "States" ComponentName
    <*> getNames scope insta nodes "Regions" ComponentName
  let hierarchy = toForest
        (S.toAscList $ S.union compositeContains regionContains)
        $ flip Node [] <$> S.toAscList (toSet nodes)
      names = M.fromList $ zip (nubOrd $ M.elems componentNames) $ pure <$> ['A'..]
      getName x = fromMaybe "" $ M.lookup x componentNames >>= (`M.lookup` names)
  (starts, conns) <- S.partition (isStartNode . (\(x, _, _) -> x))
    <$> getConnections scope insta nodes
  let sMap = M.fromAscList $ (\(x, y, _) -> (x, y)) <$> S.toAscList starts
      (startsMap, hierarchy') = extractStartNodes sMap id Nothing hierarchy
      pathTo = getPathTo hierarchy'
      getStart x = maybeToList (join $ M.lookup x startsMap) >>= pathTo
      stateDia = forestToStateDiagram getName getStart hierarchy'
      conns' = S.map (\(x, y, z) -> (pathTo x, pathTo y, z)) conns
  return $ localise $
    let (sd::UMLStateDiagram) = ([] <$ stateDia)
    in
      case sd of
        StateDiagram{} -> sd { connection = uncurry3 Connection <$> S.toAscList conns' }
        _ -> error "not defined"
  where
    getAs
      :: (IsString s, MonadError s m, Ord a)
      => String
      -> (String -> a)
      -> m (Set a)
    getAs = getX scope insta

{-|
Get path to element in the forest. Assumes there is just one such element.
Fails if the element does not exist within the forest.
-}
getPathTo :: Eq a => Forest (a, b) -> a -> b
getPathTo x = head . catMaybes . getPathsTo x

getPathsTo :: Eq a => Forest (a, b) -> a -> [Maybe b]
getPathsTo []        _ = []
getPathsTo hierarchy x =
  map findLabel hierarchy
  ++ concatMap ((`getPathsTo` x) . subForest) hierarchy
  where
    findLabel x'
      | fst (rootLabel x') == x = Just (snd $ rootLabel x')
      | otherwise               = Nothing

isStartNode :: Node -> Bool
isStartNode StNode {} = True
isStartNode _         = False

extractStartNodes
  :: Map Node x
  -> ([Int] -> [Int])
  -> Maybe Node
  -> [Tree Node]
  -> (Map (Maybe Node) (Maybe x), [Tree (Node, [Int])])
extractStartNodes _ _ _ [] = (M.empty, [])
extractStartNodes sMap prefix mnode hierarchy =
  (foldr insertStartTarget stMap starts, hierarchy')
  where
    (starts, subs) = partition (isStartNode . rootLabel) hierarchy
    (stMap, hierarchy') = mconcat $ zipWith descend [1..] subs
    descend n x = pure . Node (rootLabel x, prefix [n]) <$>
      extractStartNodes sMap (prefix . (n:)) (Just $ rootLabel x) (subForest x)
    insertStartTarget = M.insert mnode . (`M.lookup` sMap) . rootLabel

forestToStateDiagram
  :: (Node -> String)
  -> (Maybe Node -> [Int])
  -> [Tree (Node, [Int])]
  -> StateDiagram ()
forestToStateDiagram getName getStart ts = StateDiagram {
  label = 1,
  substate = treeToStateDiagram getName (stail . getStart) <$> ts,
  name = "",
  connection = (), -- TODO
  startState = getStart Nothing
  }

{-|
Like 'tail', but never trying to shorten the empty list.
-}
stail :: [a] -> [a]
stail [] = []
stail xs = tail xs

treeToStateDiagram
  :: (Node -> String)
  -> (Maybe Node -> [Int])
  -> Tree (Node, [Int])
  -> StateDiagram ()
treeToStateDiagram getName getStart n = case node of
  CNode {} -> StateDiagram {
    label = l,
    substate = fromTree <$> subForest n,
    connection = (),
    name = getName node,
    startState = getStart (Just node)
    }
  ENode {} -> EndState { label = l }
  FJNode {} -> Joint { label = l }
  HNode (HistoryNode t _) -> History {
    label = l,
    historyType = t
    }
  RNode {} -> CombineDiagram {
    label = l,
    substate = fromTree <$> subForest n
    }
  SNode {} -> InnerMostState {
    label = l,
    name = getName node,
    operations = ""
    }
  where
    l = last $ snd root
    root = rootLabel n
    node = fst root
    fromTree = treeToStateDiagram getName (stail . getStart)

{-|
Transforms a mapping of containments into a tree structure.
It assumes a valid structure for state charts
and does not perform consistency checks of any kind.
-}
toForest :: [(Node, Node)] -> Forest Node -> Forest Node
toForest ns f = foldr reorderTree f ns
  where
    reorderTree
      :: Eq a => (a, a) -> [Tree a] -> [Tree a]
    reorderTree (x, y) xs =
      let (ma, xs') = mdelete y xs
      in maybe id (insertAt x) ma <$> xs'
    mdelete
      :: Eq a
      => a
      -> [Tree a]
      -> (Maybe (Tree a), [Tree a])
    mdelete _ [] = (Nothing, [])
    mdelete x (y:xs)
      | x == rootLabel y
      = (Just y, xs)
      | otherwise
      =  (y:) <$> mdelete x xs
    insertAt :: Eq a => a -> Tree a -> Tree a -> Tree a
    insertAt x y x'
      | x == rootLabel x'
      = x' {subForest = y:subForest x'}
      | otherwise
      = x' {subForest = insertAt x y <$> subForest x'}

toX :: (String -> a) -> String -> Int -> a
toX f x = f . (x ++) . ('$':) . show

returnX :: Monad m => (String -> a) -> String -> Int -> m a
returnX x y = return . toX x y

getX
  :: (MonadError s m, IsString s, Ord a)
  => String
  -> AlloyInstance
  -> String
  -> (String -> a)
  -> m (Set a)
getX scope insta n f =
  lookupSig (scoped scope n) insta
  >>= getSingleAs "" (returnX f)

getNames
  :: (MonadError s m, IsString s, Ord a)
  => String
  -> AlloyInstance
  -> Nodes
  -> String
  -> (String -> a)
  -> m (Set (Node, a))
getNames scope insta ns n f = do
  named <- lookupSig (scoped scope n) insta
  getDoubleAs "name" (toNode ns) (returnX f) named

getConnections
  :: (MonadError s m, IsString s)
  => String
  -> AlloyInstance
  -> Nodes
  -> m (Set (Node, Node, String))
getConnections scope insta ns = do
  triggerNames  <- lookupSig (scoped scope "TriggerNames") insta
  triggers <-  getSingleAs "" (returnX TriggerName) triggerNames
  realFlows  <- lookupSig (scoped scope "Flows") insta
  protoFlows <- lookupSig (scoped scope "ProtoFlows") insta
  flows <- getSingleAs "" (returnX Trigger) realFlows
  from <- only fst flows <$> getDoubleAs
    "from"
    (returnX Trigger)
    (toNode ns)
    protoFlows
  to <- M.fromAscList . S.toAscList . only fst flows
    <$> getDoubleAs "to" (returnX Trigger) (toNode ns) protoFlows
  tlabel <- M.fromAscList . S.toAscList . only snd triggers .  only fst flows
    <$> getDoubleAs
      "label"
      (returnX Trigger)
      (returnX TriggerName)
      realFlows
  let labelMap :: Map TriggerName String
      labelMap = M.fromAscList . zip (S.toAscList triggers) $ pure <$> ['a'..]
  return $ link to tlabel labelMap from
  where
    only f xs = S.filter $ (`S.member` xs) . f
    link to lbs lm = S.map $ \(x, f) -> (
      f,
      fromJust $ M.lookup x to,
      fromMaybe "" $ M.lookup x lbs >>= (`M.lookup` lm)
      )

getContains
  :: (MonadError s m, Ord a, IsString s)
  => String
  -> AlloyInstance
  -> Nodes
  -> String
  -> (String -> a)
  -> m (Set (a, Node))
getContains scope insta ns n f = do
  container <- lookupSig (scoped scope n) insta
  getDoubleAs "contains" (returnX f) (toNode ns) container

toNode
  :: (MonadError s m, IsString s)
  => Nodes
  -> String
  -> Int
  -> m Node
toNode ns x i = ifX CNode CompositeState cNodes
  $ ifX StNode StartNode stNodes
  $ ifX ENode EndNode eNodes
  $ ifX FJNode ForkJoinNode fjNodes
  $ ifX RNode RegionState rNodes
  $ ifX SNode State sNodes
  $ ifX HNode (HistoryNode Shallow) hNodes
  $ ifX HNode (HistoryNode Deep) hNodes
  $ throwError $ fromString $ "unknown node x$" ++ show i
  where
    ifX f g which h =
      let node = toX g x i
      in if node `S.member` which ns
         then return $ f node
         else h
