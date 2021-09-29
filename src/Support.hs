module Support where
import Datatype
import Data.Graph
import Data.Maybe
import qualified Data.Map as Map
import Data.List

betweenConnection :: ConnectionType -> ConnectionType
betweenConnection ForwardH = ForwardWH
betweenConnection ForwardWH = ForwardWH
betweenConnection BackwardH = BackwardWH
betweenConnection BackwardWH = BackwardWH
betweenConnection _ = ForwardH

buildEmptyWrapperByLayer :: [[Wrapper]] -> [[Wrapper]] -> [[Wrapper]]
buildEmptyWrapperByLayer [_, _] list = list ++ [[]]
buildEmptyWrapperByLayer xs list = buildEmptyWrapperByLayer (tail xs)
  (list ++ [[]])

buildEmptyConnectionByLayer :: [[Wrapper]] -> [[ConnectWithType]] ->
  [[ConnectWithType]]
buildEmptyConnectionByLayer [_, _] list = list ++ [[]]
buildEmptyConnectionByLayer xs list = buildEmptyConnectionByLayer (tail xs)
  (list ++ [[]])

checkWrapperLayer :: Wrapper -> Bool
checkWrapperLayer StartS {} = True
checkWrapperLayer CrossStateDummy {} = True
checkWrapperLayer _ = False

checkOrList' :: Wrapper -> Bool
checkOrList' OrDecom {} = True
checkOrList' AndDecom {} = True
checkOrList' _ = False

checkOrList :: [Wrapper] -> Bool
checkOrList = any checkOrList'

{-
checkTransitionLayer' :: Wrapper -> Bool
checkTransitionLayer' Transition {} = True
checkTransitionLayer' Dummy {} = True
checkTransitionLayer' _ = False

checkTransitionLayer :: [Wrapper] -> Bool
checkTransitionLayer = all checkTransitionLayer'
-}

combineWrapper :: [[Wrapper]] -> [[Wrapper]] -> [[Wrapper]] -> [[Wrapper]]
combineWrapper [a, b] [c] newWrapper = newWrapper ++ [a, c, b]
combineWrapper (x:xs) (y:ys) newWrapper = combineWrapper xs ys (newWrapper ++
  [x, y])
combineWrapper _ _ _ = []

decideConnectionType :: Int -> Int -> ConnectionType
decideConnectionType startLayer endLayer
  | startLayer > endLayer = BackwardH
  | startLayer < endLayer = ForwardH
  | otherwise = SelfCL

filterConnection :: [Connection] -> [Connection] -> [Connection] -- connection within same depth
filterConnection [] [] = []
filterConnection a@(_:_) [] = a
filterConnection [] (x:xs)
  | length (pointTo x) == 1 && length (pointFrom x) == 1 = filterConnection [x]
    xs
  | otherwise = filterConnection [] xs
filterConnection a (x:xs)
  | length (pointTo x) == 1 && length (pointFrom x) == 1 = filterConnection (a
    ++ [x]) xs
  | otherwise = filterConnection a xs

filterCrossStateConnection :: [ConnectWithType] -> [ConnectWithType] ->
  [ConnectWithType] -> ([ConnectWithType], [ConnectWithType ])
filterCrossStateConnection [] cross nonCross = (cross, nonCross)
filterCrossStateConnection (x:xs) cross nonCross =
  case (pointFrom $ connecting x, pointTo $ connecting x) of
    ([_], [_]) -> filterCrossStateConnection xs cross (nonCross ++ [x])
    (_, _) -> filterCrossStateConnection xs (cross ++ [x]) nonCross

findLayer :: Int -> [[Wrapper]] -> Int -> Int
findLayer _ [] _ = 0
findLayer num (x:xs) layer
  | num `elem` fmap key x = layer
  | otherwise = findLayer num xs (layer + 1)

getCompareList :: Bool -> Wrapper -> [[Int]]
getCompareList checkType wrapper = case wrapper of
  OrDecom {} -> if checkType then fmap ((\ a -> key wrapper : [a]) . key)
    (head $ layered wrapper) else fmap ((\ a -> key wrapper : [a]) . key)
    (last $ layered wrapper)
  AndDecom {} -> fmap (key wrapper :) (concatMap (getCompareList checkType)
    (component wrapper))
  _ -> []

getConnection :: [Int] -> [ConnectWithType] -> [(([Int], [Int]), ConnectionType
  )]
getConnection a = fmap (\ (ConnectWithType x y) -> ((a ++ pointFrom x, a ++
  pointTo x), y))

getConnectionWithLayerBefore2 :: Wrapper -> [ConnectWithType] -> [[Int]] -> [[Int]]
getConnectionWithLayerBefore2 _ [] connected = connected
getConnectionWithLayerBefore2 wrapper (x:xs) connected
  | head (pointFrom y) == key wrapper = getConnectionWithLayerBefore2 wrapper xs
      (connected ++ [pointTo y])
  | head (pointTo y) == key wrapper = getConnectionWithLayerBefore2 wrapper xs
      (connected ++ [pointFrom y])
  | otherwise = getConnectionWithLayerBefore2 wrapper xs connected
  where
    y = connecting x

getConnectionWithLayerBefore3 :: [Int] -> [ConnectWithType] -> [[Int]] -> [[Int]]
getConnectionWithLayerBefore3 _ [] connected = connected
getConnectionWithLayerBefore3 wrapper (x:xs) connected
  | pointFrom y == wrapper = getConnectionWithLayerBefore3 wrapper xs
      (connected ++ [pointTo y])
  | pointTo y == wrapper = getConnectionWithLayerBefore3 wrapper xs
      (connected ++ [pointFrom y])
  | otherwise = getConnectionWithLayerBefore3 wrapper xs connected
  where
    y = connecting x

getDeeperConnection :: [Int] -> Wrapper -> [Int]
getDeeperConnection (a:b:_) wrapper = case returnState (concat $ layered
  wrapper) a of
  s@OrDecom {} -> a : [maxLabel s + 1]
  s@AndDecom {} -> a : b : [maxLabel (returnState (component s) b) + 1]
  _ -> []
getDeeperConnection _ _ = []

getDeeperState :: Wrapper -> [Int] -> Wrapper -- given a wrapper and label within layered
getDeeperState a [x] = returnState (concat $ layered a) x
getDeeperState a@AndDecom {} (x:xs) = getDeeperState (returnState (component a)
  x) xs
getDeeperState a (x:xs) = getDeeperState (returnState (concat $ layered a) x) xs
getDeeperState _ [] = StartS (-1) 0 NoConnection Unspecified

getDeeperLevelL :: ([Int], [Int]) -> Int -> [Wrapper] -> Int
getDeeperLevelL ([a], [b]) commonValue layersBef = higherIndex (a, b) (fmap key
  newLayerBef)
  where
      newState = returnState layersBef commonValue
      newLayerBef = last $ layered newState
getDeeperLevelL ([a, as], [b, bs]) commonValue layersBef = if a /= b then
  higherIndex (a, b) (fmap key (component newState)) else higherIndex (as, bs)
  newAndLayerBef
  where
    newState = returnState layersBef commonValue
    newState' = returnState (component newState) a
    newAndLayerBef = fmap key (last (layered newState'))
getDeeperLevelL _ _ _ = 0

getDeeperLevelR :: ([Int], [Int]) -> Int -> [Wrapper] -> Int
getDeeperLevelR ([a], [b]) commonValue layersBef = higherIndex (a, b) (fmap key
  newLayerBef)
  where
      newState = returnState layersBef commonValue
      newLayerBef = head $ layered newState
getDeeperLevelR ([a, as], [b, bs]) commonValue layersBef = if a /= b then
  higherIndex (a, b) (fmap key (component newState)) else higherIndex (as, bs)
  newAndLayerBef
  where
    newState = returnState layersBef commonValue
    newState' = returnState (component newState) a
    newAndLayerBef = fmap key (head (layered newState'))
getDeeperLevelR _ _ _ = 0

getFirstFromTuple3 :: (a, b, c) -> a
getFirstFromTuple3 (a, _, _) = a

getSecondFromTuple3 :: (a, b, c) -> b
getSecondFromTuple3 (_, b, _) = b

getThirdFromTuple3 :: (a, b, c) -> c
getThirdFromTuple3 (_, _, c) = c

getOrderedList :: [UMLStateDiagram] -> [Int]
getOrderedList a = originalOrderIndex
  where
    originalOrder = fmap label a
    ascOrder = sort originalOrder
    mapIndex = mapWithIndex ascOrder 0 Map.empty
    originalOrderIndex = fmap (mapIndex Map.!) originalOrder

getUMLStateDiagram :: Int -> (Graph, Vertex -> (UMLStateDiagram, Int, [Int]),
  Int -> Maybe Vertex) -> UMLStateDiagram
getUMLStateDiagram vertex graphFE = getFirstFromTuple3 (getSecondFromTuple3
  graphFE vertex)

higherIndex :: (Int, Int) -> [Int] -> Int
higherIndex (fstValue, sndValue) list
  | fromJust (elemIndex fstValue list) > fromJust (elemIndex sndValue list) = 1
  | otherwise = 0

higherIndex2 :: ([Int], [Int]) -> [Wrapper] -> Bool -> Int
higherIndex2 ([a], [b]) layersBef _ = higherIndex (a, b) (fmap key layersBef)
higherIndex2 (a:as , b:bs) layersBef checkType
  | a /= b = higherIndex (a, b) (fmap key layersBef)
  | checkType = getDeeperLevelL (as, bs) a layersBef
  | otherwise = getDeeperLevelR (as, bs) a layersBef
higherIndex2 _ _ _ = 0

mapWithIndex :: [Int] -> Int -> Map.Map Int Int -> Map.Map Int Int
mapWithIndex [] _ a = a
mapWithIndex (x:xs) ind ys = mapWithIndex xs (ind + 1) (Map.insert x ind ys)

mapWithLabel :: [UMLStateDiagram] -> Map.Map Int UMLStateDiagram
mapWithLabel a = Map.fromList (fmap (\ x -> (label x, x)) a)

mapWithConnection :: [Connection] -> Map.Map Int [Int]
mapWithConnection
  = foldl (\ a x -> Map.insertWith (++) (head (pointFrom x)) (pointTo x) a)
    Map.empty

mapWithConnection' :: [UMLStateDiagram] -> Map.Map Int [Int] -> Map.Map Int
  [Int]
mapWithConnection' [] a = a
mapWithConnection' (x:xs) a
  | Map.member (label x) a = mapWithConnection' xs a
  | Map.notMember (label x) a = mapWithConnection' xs (Map.insert (label x) []
      a)
  | otherwise = Map.empty

moveToFirst :: [UMLStateDiagram] -> Int -> [UMLStateDiagram] ->
  [UMLStateDiagram]
moveToFirst [] _ _ = []
moveToFirst (x:xs) a newOrder
  | a == label x = [x] ++ newOrder ++ xs
  | otherwise = moveToFirst xs a (newOrder ++ [x])

returnState :: [Wrapper] -> Int -> Wrapper
returnState (x:xs) matchLabel
  | key x == matchLabel = x
  | otherwise = returnState xs matchLabel
returnState [] _ = StartS (-1) 0 NoConnection Unspecified
