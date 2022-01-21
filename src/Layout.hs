module Layout
  ( drawDiagram
  , checkWrapper
  ) where

import Datatype
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Data.Map as Map
import Data.Graph
import Data.Tree
import Data.List.Index
import Arrows
import Support

drawDiagram :: UMLStateDiagram -> Diagram B
drawDiagram = drawWrapper' [] . orderFunction

textBox :: String -> [String] -> [String]
textBox a stringList
  | length a < 16 = stringList ++ [a] -- set text width here
  | otherwise = textBox (snd splittingText) (stringList ++ [fst splittingText])
    where
      splittingText = splitAt 15 a

drawTextBox :: String -> Diagram B
drawTextBox s = text s # font "Monospace" # fontSizeL 0.2 <> rect (0.11 *
  multiplier) 0.19 # lcA transparent
  where
    multiplier = fromIntegral $ length s

drawText :: String -> Diagram B
drawText s = drawing <> rect (width drawing) (height drawing) # lcA transparent
  where
    drawing = vcat (fmap (alignL . drawTextBox) (textBox s [])) # centerXY

drawSwimlane :: Bool -> [Diagram B] -> Double -> [Diagram B]
drawSwimlane False [a, b] c = [a, hrule c # dashingG [0.1] 0 , b] -- vertical AND
drawSwimlane False (x:xs) c = [x, hrule c # dashingG [0.1] 0] ++
  drawSwimlane False xs c
drawSwimlane True [a, b] c = [a, vrule c # dashingG [0.1] 0 , b] -- horizontal AND
drawSwimlane True (x:xs) c = [x, vrule c # dashingG [0.1] 0] ++
  drawSwimlane True xs c
drawSwimlane _ _ _ = []

drawSwimlane' :: [Diagram B] -> Diagram B
drawSwimlane' [a, b] = vcat [a, hrule c, b]
  where
    c = if width a > width b then width a else width b
drawSwimlane' _ = circle 1

appendEdges :: Diagram B -> Double -> RightConnect -> Layout -> Diagram B
appendEdges a 0 _ _ = a
appendEdges a _ NoConnection _ = a
appendEdges a l rightType layouts = case layouts of
  Vertical -> if l > 2 then (a === ((rect w1 l # lcA transparent) <>
    arrowAt' (if rightType == WithArrowhead then arrowStyle1 else arrowStyle2)
    (p2 (0, 0.5 * (-l))) (r2 (0, l)))) # centerXY else a
  _ -> if l > 2 then
    (a ||| ((rect l h1 # lcA transparent) <> arrowAt'
    (if rightType == WithArrowhead then arrowStyle1 else arrowStyle2)
    (p2 (0.5 * l, 0)) (r2 (-l, 0)))) # centerXY else a
  where
    w1 = width a
    h1 = height a

drawWrapper :: [Int] -> Wrapper -> Diagram B
drawWrapper a (StartS _ l rightType layouts) = appendEdges (circle 0.1 # fc
  black) l rightType layouts # named (a ++ [-1])
drawWrapper a (CrossStateDummy b _) = square 0.005 # fc black # named (a ++ [b])
drawWrapper a (EndS k l rightType layouts) =
  appendEdges (circle 0.16 # lw ultraThin `atop` circle 0.1 # fc black) l rightType layouts
  # named (a ++ [k])
drawWrapper a (Leaf b c d l rightType layouts) = if d == "" then appendEdges
  (text' <> roundedRect (width text' + 0.3) (height text' + 0.3) 0.1 # lc black) l
  rightType layouts # named (a ++ [b]) else appendEdges drawing'
  l rightType layouts #
  named (a ++ [b])
  where
    text' = drawText c
    text'' = drawText d
    drawing = drawSwimlane' [text' <> roundedRect (width text' + 0.3) (height text' + 0.3) 0.1
      # lcA transparent, text'' <> roundedRect (width text'' + 0.3) (height text'' + 0.3) 0.1 ] # centerXY
    drawing' = (roundedRect (width drawing) (height drawing) 0.1 <> drawing) #
      centerXY
drawWrapper a (Hist b histType l rightType layouts) = appendEdges ((if
  histType == Deep then drawText "H*" else drawText "H") <> circle 0.25
  # lc black) l rightType layouts # named (a ++ [b])
drawWrapper a (Fork b layouts l rightType) = if layouts == Vertical then
  appendEdges (rect 1 0.1 # fc black) l rightType Horizontal # named (a ++
  [b]) else appendEdges (rect 0.1 1 # fc black) l rightType Vertical # named
  (a ++ [b])
drawWrapper a (Dummy b Unspecified w) = ((rect w 0.5 # lcA transparent) <>
  arrowAt' arrowStyle2 (p2 (-0.5 * w, 0)) (r2 (w, 0))) # named (a ++ [b])
drawWrapper a (Dummy b Horizontal w) = ((rect w 0.5 # lcA transparent) <>
  arrowAt' arrowStyle2 (p2 (-0.5 * w, 0)) (r2 (w, 0))) # named (a ++ [b])
drawWrapper a (Dummy b Vertical h) = ((rect 0.5 h # lcA transparent) <>
  arrowAt' arrowStyle2 (p2 (0, -0.5 * h)) (r2 (0, h))) # named (a ++ [b])
drawWrapper a (Transition b c l rightType layouts) = appendEdges
  (x <> rect (width x + 0.1) (height x + 0.1) # lcA transparent) l rightType
  layouts # named (a ++ [b])
  where
    x = drawText c
drawWrapper a s@AndDecom {} = appendEdges (roundedRect (width f) (height f) 0.1
  <> f) (lengthXY s) (rightC s) (outerLayout s) # named (a ++
  [key s])
  where
    w = maximum (fmap (width . drawWrapper (a ++ [key s])) (component s))
    h = maximum (fmap (height . drawWrapper (a ++ [key s])) (component s))
    d = (vcat . fmap alignL) (drawSwimlane False (fmap (drawWrapper' (a ++ [key
      s])) (component s)) w) # centerXY
    e = (hcat . fmap alignT) (drawSwimlane True (fmap (drawWrapper' (a ++ [key
      s])) (component s)) h) # centerXY
    f = if layout s == Vertical then e else d
drawWrapper d s@OrDecom {} = appendEdges (roundedRect (width draw + 0.9)
  (height draw + 0.9) 0.1 # lc black <> text' # alignTL # translate
  (r2(-0.5 * (width draw + 0.9) + 0.1 , 0.5 * (height draw + 0.9) - 0.1)) <> draw # centerXY) (lengthXY s) (rightC s)
  (outerLayout s) # named (d ++ [key s]) # applyAll (fmap (`drawConnection`
  layout s) connectList)
  where
    text' = drawTextBox (strings s)
    draw = drawLayers (layout s == Vertical) (d ++ [key s]) (layered s)
    connectList = getConnection (d ++ [key s]) (connections s)

drawWrapper' :: [Int] -> Wrapper -> Diagram B
drawWrapper' d s@OrDecom {} = appendEdges (roundedRect (width draw + 0.9)
  (height draw + 0.9) 0.1 # lcA transparent <> alignedText 0 1 (strings s) #
  translate (r2(-0.485 * (width draw + 0.9), 0.485 * (height draw + 0.9))) #
  fontSize (local 0.2) <> draw # centerXY) (lengthXY s) (rightC s)
  (outerLayout s) # named (d ++ [key s]) # applyAll (fmap (`drawConnection`
  layout s) connectList)
  where
    draw = drawLayers (layout s == Vertical) (d ++ [key s]) (layered s)
    connectList = getConnection (d ++ [key s]) (connections s)
drawWrapper' _ _ = mempty

drawLayer :: Bool -> [Int] -> [Wrapper] -> Diagram B
drawLayer True prefix layer = vsep 0.5 (fmap (alignL . drawWrapper prefix)
  layer) # centerXY
drawLayer False prefix layer = hsep 0.5 (fmap (alignT . drawWrapper prefix)
  layer) # centerXY

drawLayers :: Bool -> [Int] -> [[Wrapper]] -> Diagram B
drawLayers False prefix layers = hsep 0.5 (fmap (drawLayer True prefix) layers)
drawLayers True prefix layers = vsep 0.5 (fmap (drawLayer False prefix) layers)

drawConnection :: (([Int], [Int]), ConnectionType) -> Layout -> Diagram B ->
  Diagram B
drawConnection (a, ForwardH) Vertical = uncurry downwardArrowWithHead a
drawConnection (a, ForwardH) _ = uncurry forwardArrowWithHead a
drawConnection (a, ForwardWH) Vertical = uncurry downwardArrowWithoutHead a
drawConnection (a, ForwardWH) _ = uncurry forwardArrowWithoutHead a
drawConnection (a, BackwardH) Vertical = uncurry upwardArrowWithHead a
drawConnection (a, BackwardH) _ = uncurry backwardArrowWithHead a
drawConnection (a, BackwardWH) Vertical = uncurry upwardArrowWithoutHead a
drawConnection (a, BackwardWH) _ = uncurry backwardArrowWithoutHead a
drawConnection (a, SelfCL) Vertical = uncurry selfConnect4 a
drawConnection (a, SelfCL) _ = uncurry selfConnect2 a
drawConnection (a, SelfCR) Vertical = uncurry selfConnect3 a
drawConnection (a, SelfCR) _ = uncurry selfConnect1 a

{-
selectSmallerSize :: Wrapper -> Layout -- for OrDecom
selectSmallerSize a = if areaH > areaV then Vertical else Horizontal
  where
    v = drawWrapper [] (changeLayout a Vertical)
    h = drawWrapper [] (changeLayout a Horizontal)
    areaV = width v * height v
    areaH = width h * height h

decideAndLayout :: Wrapper -> Layout
decideAndLayout a = if areaV < areaH then Vertical else Horizontal
  where
    v = AndDecom (fmap (`changeAndLayout` Vertical) (component a)) (key a)
      Vertical (lengthXY a) (rightC a) (outerLayout a)
    h = AndDecom (fmap (`changeAndLayout` Horizontal) (component a)) (key a)
      Horizontal (lengthXY a) (rightC a) (outerLayout a)
    areaV = width (drawWrapper [] v) * height (drawWrapper [] v)
    areaH = width (drawWrapper [] h) * height (drawWrapper [] h)
-}

getWrapper :: UMLStateDiagram -> Wrapper
getWrapper = toWrapper . localise

toWrapper :: UMLStateDiagram -> Wrapper
toWrapper (EndState a ) = EndS a 0 NoConnection Unspecified
toWrapper (History a b) = Hist a b 0 NoConnection Unspecified
toWrapper (InnerMostState a b c) = Leaf a b c 0 NoConnection Unspecified
toWrapper (Joint a) = Fork a Unspecified 0 NoConnection
toWrapper s@CombineDiagram {} = AndDecom (fmap toWrapper (substate s)) (label
  s) Unspecified 0 NoConnection Unspecified
toWrapper s@StateDiagram {} = OrDecom toWrapper' (label s) (name s)
  convertedConnection Unspecified maxKey 0 NoConnection
  Unspecified
  where
    vertexOrder = getOrderedList (substate s)
    mapConnection = mapWithConnection' (substate s) (mapWithConnection
      (filterConnection [] (connection s)))
    graphWithInfo = createGraph mapConnection (substate s) []
    graph = getFirstFromTuple3 graphWithInfo
    getLayers = layering $ dfs graph vertexOrder
    layers = nodeFromVertex getLayers graphWithInfo []
    toWrapper' = if null (startState s) then convertUMLStateDiagramToWrapper
      layers [] else placeStartState (convertUMLStateDiagramToWrapper layers
      []) (startState s)
    newConnection = if null (startState s) then connection s
      else Connection [-1] (startState s) "" : connection s
    convertedConnection = changeConnectionType newConnection toWrapper' []
    maxKey = maximum (Map.keys $ mapWithLabel $ substate s)

createGraph :: Map.Map Int [Int] -> [UMLStateDiagram] -> [(UMLStateDiagram,
  Int, [Int])] -> (Graph, Vertex -> (UMLStateDiagram, Int, [Int]), Int -> Maybe
  Vertex)
createGraph _ [] a = graphFromEdges a
createGraph c1 (x:xs) a = createGraph c1 xs (a ++ [(x, label
  x, c1 Map.! label x)])

returnNodeFromVertex :: [Int] -> (Graph, Vertex -> (UMLStateDiagram, Int, [Int]
  ), Int -> Maybe Vertex) -> [UMLStateDiagram] -> [UMLStateDiagram]
returnNodeFromVertex [] _ a = a
returnNodeFromVertex (x:xs) graphFE listSD = returnNodeFromVertex xs graphFE
  (listSD ++ [getUMLStateDiagram x graphFE])

nodeFromVertex :: [[Int]] -> (Graph, Vertex -> (UMLStateDiagram, Int, [Int]),
  Int -> Maybe Vertex) -> [[UMLStateDiagram]] -> [[UMLStateDiagram]]
nodeFromVertex [] _ a = a
nodeFromVertex (x:xs) graphFE listSD = nodeFromVertex xs graphFE (listSD ++
  [returnNodeFromVertex x graphFE []])

layering :: Forest Vertex -> [[Vertex]]
layering = foldl (\ b x -> b ++ levels x) []

convertUMLStateDiagramToWrapper :: [[UMLStateDiagram]] -> [[Wrapper]] ->
  [[Wrapper]]
convertUMLStateDiagramToWrapper [] x = x
convertUMLStateDiagramToWrapper (x:xs) layers =
  convertUMLStateDiagramToWrapper xs (layers ++ [fmap getWrapper x])

placeStartState :: [[Wrapper]] -> [Int] -> [[Wrapper]]
placeStartState [[a]] _ = [StartS (-1) 0 NoConnection Unspecified] : [[a]]
placeStartState originalW ss =  modifyAt layerToInsert (++ [StartS (-1) 0
  NoConnection Unspecified]) originalW
  where
    ssLayer = findLayer (head ss) originalW 0
    layerToInsert = if ssLayer == (length originalW -1) then ssLayer - 1 else
      ssLayer + 1

addDummy :: Wrapper -> Wrapper
addDummy s@OrDecom {} = case layered s of
  [[_]] -> OrDecom (fmap (fmap addDummy) (layered s))
    (key s) (strings s) (connections s) (layout s) (maxLabel s) (lengthXY s)
    (rightC s) (outerLayout s)
  _ -> OrDecom loopDummy (key s) (strings s)
    connectionWithTransition Unspecified (getSecondFromTuple3 withTransition)
    (lengthXY s) (rightC s) (outerLayout s)
    where
      withDummy = addDummyStates (maxLabel s) (layered s) (connections s) []
      withTransition = addTransitionStates (getSecondFromTuple3 withDummy)
        (buildEmptyWrapperByLayer (getFirstFromTuple3 withDummy) [])
        (getFirstFromTuple3 withDummy) (getThirdFromTuple3 withDummy) []
      stateWithTransition = getFirstFromTuple3 withTransition
      connectionWithTransition = getThirdFromTuple3 withTransition
      {- withTransition = addTransitionStates (maxLabel s) (buildEmptyWrapperByLayer
        (layered s) []) (layered s) (connections s) []
      withDummy = addDummyStates (getSecondFromTuple3 withTransition) (getFirstFromTuple3
        withTransition) (getThirdFromTuple3 withTransition) []
      stateWithDummy = getFirstFromTuple3 withDummy
      connectionWithDummy = getThirdFromTuple3 withDummy -}
      loopDummy = fmap (fmap addDummy) stateWithTransition
addDummy s@AndDecom {} = AndDecom loopDummy (key s) Unspecified (lengthXY s)
  (rightC s) (outerLayout s)
  where
    loopDummy = fmap addDummy (component s)
addDummy a = a

addD :: ConnectionType -> Int -> [Int] -> [[Wrapper]] -> ConnectWithType ->
  [ConnectWithType] -> ([[Wrapper]], Int, [ConnectWithType])
addD a maxKey [_, _] withDummy c@ConnectWithType {} withConnect = (withDummy,
  maxKey, withConnect ++ [ConnectWithType (Connection [maxKey] (pointTo $
  connecting c) (transition $ connecting c)) a])
addD a maxKey (_:xs) withDummy c@ConnectWithType {} [] =
  addD a (maxKey + 1) xs (modifyAt (head xs) (\ x -> x ++ [Dummy (maxKey + 1)
  Unspecified 0.1]) withDummy) c [ConnectWithType (Connection (pointFrom
  $ connecting c) [maxKey + 1] "") (betweenConnection a)]
addD a maxKey (_:xs) withDummy c@ConnectWithType {} withConnect =
  addD a (maxKey + 1) xs (modifyAt (head xs) (\ x -> x ++ [Dummy (maxKey + 1)
  Unspecified 0.1]) withDummy) c (withConnect ++ [ConnectWithType (
  Connection [maxKey] [maxKey + 1] "") (betweenConnection a)])
addD _ _ _ _ _ _ = ([], 0, [])

addDummyStates :: Int -> [[Wrapper]] -> [ConnectWithType] -> [ConnectWithType]
  -> ([[Wrapper]], Int, [ConnectWithType])
addDummyStates maxKey withDummy [] withConnection = (withDummy, maxKey,
  withConnection)
addDummyStates maxKey withDummy (x:xs) withConnection =
  case (pointFrom (connecting x), pointTo (connecting x)) of
    (a:_, b:_)
      | layerGap == 0 -> addDummyStates maxKey withDummy xs
          (withConnection ++ [x])
      | layerGap == 1 -> addDummyStates maxKey withDummy xs (withConnection
          ++ [x])
      | startLayer > endLayer -> addDummyStates (getSecondFromTuple3 dummy')
          (getFirstFromTuple3 dummy') xs (withConnection ++ getThirdFromTuple3
          dummy')
      | otherwise -> addDummyStates (getSecondFromTuple3 dummy)
          (getFirstFromTuple3 dummy) xs (withConnection ++ getThirdFromTuple3
          dummy)
        where
          startLayer = findLayer a withDummy 0
          endLayer = findLayer b withDummy 0
          layerGap = abs (startLayer - endLayer)
          dummy = addD (connectType x) maxKey [startLayer..endLayer] withDummy
            x []
          dummy' = addD (connectType x) maxKey [startLayer, (startLayer - 1)..
            endLayer]
            withDummy x []
    (_, _) -> addDummyStates maxKey withDummy xs withConnection

addTransitionStates :: Int -> [[Wrapper]] -> [[Wrapper]] -> [ConnectWithType]
  -> [ConnectWithType] -> ([[Wrapper]], Int, [ConnectWithType])
addTransitionStates maxKey transitionLayer originalLayer [] newConnection =
  (combineWrapper originalLayer transitionLayer [], maxKey, newConnection)
addTransitionStates maxKey transitionLayer originalLayer (x:xs) newConnection =
  case connectType x of
    ForwardH -> addTransitionStates k
      (modifyAt startLayer addState transitionLayer)
      originalLayer xs (newConnection ++ [ConnectWithType (Connection
      startLabel [k] "") ForwardWH, ConnectWithType (Connection [k] endLabel
      connectName) ForwardH])
    ForwardWH -> addTransitionStates k
      (modifyAt startLayer addState transitionLayer)
      originalLayer xs (newConnection ++ [ConnectWithType (Connection
      startLabel [k] "") ForwardWH, ConnectWithType (Connection [k] endLabel
      connectName) ForwardWH])
    BackwardWH -> addTransitionStates k
      (modifyAt endLayer addState transitionLayer)
      originalLayer xs (newConnection ++ [ConnectWithType (Connection
      startLabel [k] "") BackwardWH, ConnectWithType (Connection [k] endLabel
      connectName) BackwardWH])
    BackwardH -> addTransitionStates k
      (modifyAt endLayer addState transitionLayer)
      originalLayer xs (newConnection ++ [ConnectWithType (Connection
      startLabel [k] "") BackwardWH, ConnectWithType (Connection [k] endLabel
      connectName) BackwardH])
    _
      | startLabel == endLabel -> if endLayer == length transitionLayer then
          addTransitionStates k (modifyAt (endLayer - 1) addState
          transitionLayer) originalLayer xs (newConnection ++ [ConnectWithType
          (Connection startLabel [k] connectName) SelfCR]) else
          addTransitionStates k (modifyAt endLayer addState transitionLayer)
          originalLayer xs (newConnection ++ [ConnectWithType (Connection
          startLabel [k] connectName) SelfCL])
      | otherwise -> if endLayer == length transitionLayer then
          addTransitionStates k (modifyAt (endLayer - 1) addState
          transitionLayer) originalLayer xs (newConnection ++ type1) else
          addTransitionStates k (modifyAt endLayer addState transitionLayer)
          originalLayer xs (newConnection ++ type2)
  where
    startLayer = findLayer (head $ pointFrom $ connecting x) originalLayer 0
    endLayer = findLayer (head $ pointTo $ connecting x) originalLayer 0
    startLabel = pointFrom $ connecting x
    endLabel = pointTo $ connecting x
    connectName = transition (connecting x)
    addState = if connectName == "" then (++ [Dummy k Unspecified 0.1])
      else (++ [Transition k connectName 0 NoConnection Unspecified])
    k = maxKey + 1
    type1 = case (startLabel, endLabel) of
      ([], []) -> [ConnectWithType (Connection startLabel [k] "") BackwardWH,
        ConnectWithType (Connection [k] endLabel connectName) ForwardH]
      _ -> [ConnectWithType (Connection startLabel [k] "") BackwardWH,
        ConnectWithType (Connection [k] endLabel connectName) ForwardWH]
    type2 = case (startLabel, endLabel) of
      ([], []) -> [ConnectWithType (Connection startLabel [k] "") ForwardWH,
        ConnectWithType (Connection [k] endLabel connectName) BackwardH]
      _ -> [ConnectWithType (Connection startLabel [k] "") ForwardWH,
        ConnectWithType (Connection [k] endLabel connectName) BackwardWH]

connectionsByLayers :: [ConnectWithType] -> [[Wrapper]] -> [[ConnectWithType]]
  -> [[ConnectWithType]]
connectionsByLayers [] _ connectionLayers = connectionLayers
connectionsByLayers (x:xs) layers connectionLayers =
  case (pointFrom (connecting x), pointTo (connecting x)) of
    (a:_, b:_)
      | startLayer > endLayer -> connectionsByLayers xs layers (modifyAt
          endLayer (++ [x]) connectionLayers)
      | otherwise -> connectionsByLayers xs layers (modifyAt startLayer (++ [x]
          ) connectionLayers)
        where
          startLayer = findLayer a layers 0
          endLayer = findLayer b layers 0
    (_, _) -> connectionsByLayers xs layers connectionLayers

startStateFirst :: UMLStateDiagram -> [Int] -> UMLStateDiagram
startStateFirst a [] = a
startStateFirst a@StateDiagram {} (x:xs) =
  StateDiagram (loopOrder : tail newOrder) (label a) (name a) (connection a)
  (startState a)
  where
    newOrder = moveToFirst (substate a) x []
    loopOrder = startStateFirst (head newOrder) xs
startStateFirst a@CombineDiagram {} (x:xs) =
  CombineDiagram (loopOrder : tail newOrder) (label a)
  where
    newOrder = moveToFirst (substate a) x []
    loopOrder = startStateFirst (head newOrder) xs
startStateFirst a _ = a

rearrangeSubstate :: UMLStateDiagram -> UMLStateDiagram
rearrangeSubstate s@StateDiagram {} = case startState s of
  [] -> StateDiagram (fmap rearrangeSubstate (substate s)) (label s) (name s)
    (connection s) (startState s)
  _ -> StateDiagram (fmap rearrangeSubstate (substate n)) (label n) (name s)
    (connection n) (startState n)
  where
    n = startStateFirst s (startState s)
rearrangeSubstate s@CombineDiagram {} =
  CombineDiagram (fmap rearrangeSubstate (substate s)) (label s)
rearrangeSubstate a = a

{-
changeLayout :: Wrapper -> Layout -> Wrapper
changeLayout s@OrDecom {} a = OrDecom (layered s) (key s) (strings s)
  (connections s) a (maxLabel s) (lengthXY s) (rightC s)
  (outerLayout s)
changeLayout s _ = s

changeOrLayout :: Wrapper -> Layout -> Wrapper
changeOrLayout s@OrDecom {} b = case layered s of
  [[a@AndDecom {}]] -> OrDecom (layered s) (key s) (strings s) (connections s)
    (layout a) (maxLabel s) (lengthXY s) (rightC s) b
  _ -> OrDecom (layered s) (key s) (strings s) (connections s)
    (layout s) (maxLabel s) (lengthXY s) (rightC s) b
changeOrLayout s@Fork {} a = Fork (key s) a (lengthXY s) (rightC s)
changeOrLayout s@Dummy {} a = Dummy (key s) a (lengthXY s)
changeOrLayout s@AndDecom {} a = AndDecom (component s) (key s) (layout s)
  (lengthXY s) (rightC s) a
changeOrLayout s@Hist {} a = Hist (key s) (history s) (lengthXY s)
  (rightC s) a
changeOrLayout s@EndS {} a = EndS (key s) (lengthXY s) (rightC s) a
changeOrLayout s@Leaf {} a = Leaf (key s) (strings s) (operation s) (lengthXY s
  ) (rightC s) a
changeOrLayout s@StartS {} a = StartS (key s) (lengthXY s) (rightC s) a
changeOrLayout s@Transition {} a = Transition (key s) (transitionName s)
  (lengthXY s) (rightC s) a
changeOrLayout s@CrossStateDummy {} _ = s

changeAndLayout :: Wrapper -> Layout -> Wrapper
changeAndLayout a b = if layout a == b then a else
  case layered a of
    [[s@AndDecom {}]] -> OrDecom [[AndDecom (fmap (`changeAndLayout` b)
      (component s)) (key s) b (lengthXY s) (rightC s) b]]
      (key a) (strings a) (connections a) b (maxLabel a) (lengthXY a)
      (rightC a) (outerLayout a)
    _ -> OrDecom (fmap (fmap (`changeOrLayout` b)) (layered a)) (key a)
      (strings a) (connections a) b (maxLabel a) (lengthXY a)
      (rightC a) b

assignLayout :: Wrapper -> Wrapper
assignLayout s@OrDecom {} = if checkOrList (concat (layered s)) then
  newOrLayout else OrDecom newLayered (key s) (strings s) (connections s)
  (selectSmallerSize s) (maxLabel s) (lengthXY s) (rightC s)
  (outerLayout s)
  where
    newLayered = fmap (fmap (`changeOrLayout` selectSmallerSize s)) (layered s)
    newOr = OrDecom (fmap (fmap assignLayout) (layered s)) (key s) (strings s)
      (connections s) (layout s) (maxLabel s) (lengthXY s) (rightC s)
      (outerLayout s)
    newOrLayout = OrDecom (fmap (fmap (`changeOrLayout` selectSmallerSize newOr
      )) (layered newOr)) (key s) (strings s) (connections s)
      (selectSmallerSize newOr) (maxLabel s) (lengthXY s) (rightC s)
      (outerLayout s)
assignLayout s@AndDecom {} = AndDecom (fmap (`changeAndLayout` decidedLayout)
  newLayered) (key s) decidedLayout (lengthXY s) (rightC s)
  (outerLayout s)
  where
    newLayered = fmap assignLayout (component s)
    decidedLayout = decideAndLayout (AndDecom newLayered (key s) Unspecified
      (lengthXY s) (rightC s) (outerLayout s))
assignLayout a = a
-}

reduceCrossStateCrossing :: Wrapper -> Wrapper
reduceCrossStateCrossing s@OrDecom {} = if null (fst dummyToAdd) then OrDecom
  (fmap (fmap reduceCrossStateCrossing) (layered s)) (key s) (strings s)
  (connections s) (layout s) (maxLabel s) (lengthXY s) (rightC s)
  (outerLayout s) else OrDecom (fmap (fmap reduceCrossStateCrossing) (layered
  addingDummy)) (key s) (strings s) (connections addingDummy) (layout s)
  (maxLabel addingDummy) (lengthXY s) (rightC s) (outerLayout s)
  where
    dummyToAdd = filterCrossStateConnection (connections s) [] []
    newOr = OrDecom (layered s) (key s) (strings s) (snd dummyToAdd)
      Unspecified (maxLabel s) (lengthXY s) (rightC s) (outerLayout
      s)
    addingDummy = foldl addCrossSuperStateDummy newOr (fst dummyToAdd)
reduceCrossStateCrossing s@AndDecom {} = AndDecom (fmap
  reduceCrossStateCrossing (component s)) (key s) (layout s) (lengthXY s)
  (rightC s) (outerLayout s)
reduceCrossStateCrossing a = a

reduceCrossStateCrossing' :: Wrapper -> Wrapper
reduceCrossStateCrossing' s@OrDecom {} = if null (fst dummyToAdd) then OrDecom
  (fmap (fmap reduceCrossStateCrossing') (layered s)) (key s) (strings s)
  (connections s) (layout s) (maxLabel s) (lengthXY s) (rightC s)
  (outerLayout s) else addingDummy
  where
    dummyToAdd = filterCrossStateConnection (connections s) [] []
    newOr = OrDecom (fmap (fmap reduceCrossStateCrossing') (layered s)) (key s)
      (strings s) (snd dummyToAdd) (layout s) (maxLabel s) (lengthXY s)
      (rightC s) (outerLayout s)
    addingDummy = foldl addCrossSuperStateDummy newOr (fst dummyToAdd)
reduceCrossStateCrossing' s@AndDecom {} = AndDecom (fmap
  reduceCrossStateCrossing (component s)) (key s) (layout s) (lengthXY s)
  (rightC s) (outerLayout s)
reduceCrossStateCrossing' a = a

addCrossSuperStateDummy :: Wrapper -> ConnectWithType -> Wrapper
addCrossSuperStateDummy a b = case (pointFrom $ connecting b, pointTo $
  connecting b) of
  -- start > end = addDummyRight BackwardH | start < end = addDummyLeft ForwardH | start == end = if start == length - 1 then addDummyLeft ForwardH else addDummyRight BackwardH
  ([_], _:_:_)  -- pointFrom outside to inside
    | startLayer > endLayer -> addDummyRight' BackwardH endPoint
        [ConnectWithType (Connection startPoint pointToLabel connectionName)
        BackwardWH] a
    | startLayer < endLayer -> addDummyLeft' ForwardH endPoint
        [ConnectWithType (Connection startPoint pointToLabel connectionName)
        ForwardWH] a
    | otherwise -> if startLayer == length (layered a) - 1 then addDummyLeft'
        ForwardH endPoint [ConnectWithType (Connection startPoint pointToLabel
        connectionName) SelfCL] a else addDummyRight' BackwardH endPoint
        [ConnectWithType (Connection startPoint pointToLabel connectionName)
        SelfCL] a
  -- start > end = addDummyLeft BackwardWH | start < end = addDummyRight ForwardWH | start == end = if start == length - 1 then addDummyLeft BackwardWH else addDummyRight ForwardWH
  (_:_:_, [_])  -- pointFrom inside to outside
    | startLayer > endLayer -> addDummyLeft' BackwardWH startPoint
        [ConnectWithType (Connection pointFromLabel endPoint connectionName)
        BackwardH] a
    | startLayer < endLayer -> addDummyRight' ForwardWH startPoint
        [ConnectWithType (Connection pointFromLabel endPoint connectionName)
        ForwardH] a
    | otherwise -> if startLayer == length (layered a) - 1 then addDummyLeft'
        BackwardWH startPoint [ConnectWithType (Connection pointFromLabel
        endPoint connectionName) SelfCL] a else addDummyRight'
        ForwardWH startPoint [ConnectWithType (Connection pointFromLabel
        endPoint connectionName) SelfCL] a
  -- combine first 2 cases
  (_, _)
    | startLayer > endLayer -> addDummyRight' BackwardH endPoint []
        (addDummyLeft' BackwardWH startPoint [ConnectWithType (Connection
        pointFromLabel pointToLabel connectionName) BackwardWH] a)
    | startLayer < endLayer -> addDummyLeft' ForwardH endPoint []
        (addDummyRight' ForwardWH startPoint [ConnectWithType (Connection
        pointFromLabel pointToLabel connectionName) ForwardWH] a)
    | otherwise -> if startLayer == length (layered a) - 1 then addDummyLeft'
        ForwardH endPoint [] (addDummyLeft' BackwardWH startPoint
        [ConnectWithType (Connection pointFromLabel pointToLabel connectionName
        ) SelfCL] a) else addDummyRight' BackwardH endPoint [] (addDummyRight'
         ForwardWH startPoint [ConnectWithType (Connection pointFromLabel
         pointToLabel connectionName) SelfCL] a)
  where
    startLayer = findLayer (head $ pointFrom $ connecting b) (layered a) 0
    endLayer = findLayer (head $ pointTo $ connecting b) (layered a) 0
    startPoint = pointFrom $ connecting b
    endPoint = pointTo $ connecting b
    connectionName = transition $ connecting b
    pointFromLabel = getDeeperConnection (pointFrom $ connecting b) a
    pointToLabel = getDeeperConnection (pointTo $ connecting b) a


-- cType = original connectType -- connectWT = between, therefore always without head
addDummyLeft' :: ConnectionType -> [Int] -> [ConnectWithType]  -> Wrapper ->
  Wrapper
addDummyLeft' cType (x:xs) connectWT a = OrDecom loop (key a) (strings a) (
  connectWT ++ connections a) Unspecified (maxLabel a) (lengthXY a)
  (rightC a) (outerLayout a)
  where
    loop = fmap (fmap (addDummyLeft cType xs x)) (layered a)
addDummyLeft' _ _ _ _ = StartS (-1) 0 NoConnection Unspecified

-- cType = original connectType -- connectWT = between, therefore always without head
addDummyRight' :: ConnectionType -> [Int] -> [ConnectWithType]  -> Wrapper ->
  Wrapper
addDummyRight' cType (x:xs) connectWT a = OrDecom loop (key a) (strings a) (
  connectWT ++ connections a) Unspecified (maxLabel a) (lengthXY a)
  (rightC a) (outerLayout a)
  where
    loop = fmap (fmap (addDummyRight cType xs x)) (layered a)
addDummyRight' _ _ _ _ = StartS (-1) 0 NoConnection Unspecified

addDummyLeft :: ConnectionType -> [Int] -> Int -> Wrapper -> Wrapper
addDummyLeft ForwardH [a] matchLabel b = if key b == matchLabel then
  OrDecom (insertDummyLeft b) (key b) (strings b) (
  ConnectWithType (Connection [maxLabel b + 1] [a] "") ForwardH : connections
  b) Unspecified (maxLabel b + 1) (lengthXY b) (rightC b)
  (outerLayout b) else b
addDummyLeft ForwardH (x:xs) matchLabel b@OrDecom {} = if key b == matchLabel
  then OrDecom (fmap (fmap (addDummyLeft ForwardH xs x)) (
  insertDummyLeft b)) (key b) (strings b) (ConnectWithType (Connection [
  maxLabel b + 1] (getDeeperConnection (x:xs) b) "") ForwardWH : connections b)
  Unspecified (maxLabel b + 1) (lengthXY b) (rightC b) (outerLayout
  b) else b
addDummyLeft ForwardH (x:xs) matchLabel b@AndDecom {} = if key b == matchLabel
  then AndDecom (fmap (addDummyLeft ForwardH xs x) (component b)) (key b) (
  layout b) (lengthXY b) (rightC b) (outerLayout b) else b
addDummyLeft BackwardWH [a] matchLabel b = if key b == matchLabel then
  OrDecom (insertDummyLeft b) (key b) (strings b) (
  ConnectWithType (Connection [a] [maxLabel b + 1] "") BackwardWH : connections
  b) Unspecified (maxLabel b + 1) (lengthXY b) (rightC b)(outerLayout b) else b
addDummyLeft BackwardWH (x:xs) matchLabel b@OrDecom {} = if key b == matchLabel
  then OrDecom (fmap (fmap (addDummyLeft BackwardWH xs x)) (
  insertDummyLeft b)) (key b) (strings b) (ConnectWithType (Connection (
  getDeeperConnection (x:xs) b) [maxLabel b + 1] "") BackwardWH : connections b
  ) Unspecified (maxLabel b + 1) (lengthXY b) (rightC b) (
  outerLayout b) else b
addDummyLeft BackwardWH (x:xs) matchLabel b@AndDecom {} = if key b ==
  matchLabel then AndDecom (fmap (addDummyLeft BackwardWH xs x) (component b))
  (key b) (layout b) (lengthXY b) (rightC b) (outerLayout b) else b
addDummyLeft _ _ _ b = b

addDummyRight :: ConnectionType -> [Int] -> Int -> Wrapper -> Wrapper
addDummyRight BackwardH [a] matchLabel b = if key b == matchLabel then
  OrDecom (insertDummyRight b) (key b) (strings b) (
  ConnectWithType (Connection [maxLabel b + 1] [a] "") BackwardH : connections
  b) Unspecified (maxLabel b + 1) (lengthXY b) (rightC b)
  (outerLayout b) else b
addDummyRight BackwardH (x:xs) matchLabel b@OrDecom {} = if key b == matchLabel
  then OrDecom (fmap (fmap (addDummyRight BackwardH xs x)) (
  insertDummyRight b)) (key b) (strings b) (ConnectWithType (Connection [
  maxLabel b + 1] (getDeeperConnection (x:xs) b) "") BackwardWH : connections b
  ) Unspecified (maxLabel b + 1) (lengthXY b) (rightC b)
  (outerLayout b) else b
addDummyRight BackwardH (x:xs) matchLabel b@AndDecom {} = if key b ==
  matchLabel then AndDecom (fmap (addDummyRight BackwardH xs x) (component b))
  (key b) (layout b) (lengthXY b) (rightC b) (outerLayout b) else b
addDummyRight ForwardWH [a] matchLabel b = if key b == matchLabel then
  OrDecom (insertDummyRight b) (key b) (strings b) (
  ConnectWithType (Connection [a] [maxLabel b + 1] "") ForwardWH : connections
  b) Unspecified (maxLabel b + 1) (lengthXY b) (rightC b)
  (outerLayout b) else b
addDummyRight ForwardWH (x:xs) matchLabel b@OrDecom {} = if key b == matchLabel
  then OrDecom (fmap (fmap (addDummyRight ForwardWH xs x)) (
  insertDummyRight b)) (key b) (strings b) (ConnectWithType (Connection (
  getDeeperConnection (x:xs) b) [maxLabel b + 1] "") ForwardWH : connections b)
  Unspecified (maxLabel b + 1) (lengthXY b) (rightC b) (outerLayout
  b) else b
addDummyRight ForwardWH (x:xs) matchLabel b@AndDecom {} = if key b ==
  matchLabel then AndDecom (fmap (addDummyRight ForwardWH xs x) (component b))
  (key b) (layout b) (lengthXY b) (rightC b) (outerLayout b) else b
addDummyRight _ _ _ b = b


-- | 'lengthXY' of 'CrossStateDummy'
csdWidth :: Double
csdWidth = 0.05

insertDummyLeft :: Wrapper  -> [[Wrapper]]
insertDummyLeft a = if all checkWrapperLayer (head $ layered a) then
  (CrossStateDummy (maxLabel a + 1) csdWidth : head (layered a)) : tail (layered a)
  else [CrossStateDummy (maxLabel a + 1) csdWidth] : layered a

insertDummyRight :: Wrapper -> [[Wrapper]]
insertDummyRight a = if all checkWrapperLayer (last $ layered a) then init (
  layered a) ++ [last (layered a) ++ [CrossStateDummy (maxLabel a + 1) csdWidth]] else
  layered a ++ [[CrossStateDummy (maxLabel a + 1) csdWidth]]

changeConnectionType :: [Connection] -> [[Wrapper]] -> [ConnectWithType]
  -> [ConnectWithType]
changeConnectionType [] _ withType = withType
changeConnectionType (x:xs) layers withType = changeConnectionType xs layers
  (withType ++ [ConnectWithType x decideType])
  where
    startLayer = findLayer (head $ pointFrom x) layers 0
    endLayer = findLayer (head $ pointTo x) layers 0
    decideType = decideConnectionType startLayer endLayer

changeRightConnection :: Wrapper -> RightConnect -> Wrapper
changeRightConnection s@OrDecom {} a = OrDecom (layered s) (key s) (strings s)
  (connections s) (layout s) (maxLabel s) (lengthXY s) a
  (outerLayout s)
changeRightConnection s@AndDecom {} a = AndDecom (component s) (key s) (layout
  s) (lengthXY s) a (outerLayout s)
changeRightConnection s@EndS {} a = EndS (key s) (lengthXY s) a (outerLayout s)
changeRightConnection s@Fork {} a = Fork (key s) (outerLayout s) (lengthXY s) a
changeRightConnection s@Hist {} a = Hist (key s) (history s) (lengthXY s) a
  (outerLayout s)
changeRightConnection s@Leaf {} a = Leaf (key s) (strings s) (operation s)
  (lengthXY s) a (outerLayout s)
changeRightConnection s@StartS {} a = StartS (key s) (lengthXY s) a
  (outerLayout s)
changeRightConnection s@CrossStateDummy {} _ = s
changeRightConnection s@Dummy {} _ = s
changeRightConnection s@Transition {} a = Transition (key s) (transitionName s)
  (lengthXY s) a (outerLayout s)

changeRightConnection' :: RightConnect -> RightConnect -> RightConnect
changeRightConnection' originalRight toChangeRight =
  case (originalRight, toChangeRight) of
    (NoConnection, _) -> toChangeRight
    (WithoutArrowhead, WithoutArrowhead) -> WithoutArrowhead
    (WithoutArrowhead, WithArrowhead) -> WithArrowhead
    (WithArrowhead, _) -> WithArrowhead
    (_, _) -> NoConnection

changeRightConnections :: [Int] -> Int -> RightConnect -> Wrapper -> Wrapper
changeRightConnections [] matchLabel rightType s = if key s == matchLabel then
  changeRightConnection s (changeRightConnection' (rightC s) rightType) else
  s
changeRightConnections (x:xs) matchLabel rightType s@OrDecom {} = if key s ==
  matchLabel then OrDecom (fmap (fmap (changeRightConnections xs x rightType))
  (layered s)) (key s) (strings s) (connections s) (layout s) (maxLabel s)
  (lengthXY s) (rightC s) (outerLayout s) else s
changeRightConnections (x:xs) matchLabel rightType s@AndDecom {} = if key s ==
  matchLabel then AndDecom (fmap (changeRightConnections xs x rightType)
  (component s)) (key s) (layout s) (lengthXY s) (rightC s)
  (outerLayout s) else s
changeRightConnections _ _ _ s = s

markRightConnection :: Wrapper -> ConnectWithType -> Wrapper
markRightConnection a b = OrDecom (layered afterChange) (key a) (strings a)
  (connections a ++ [ConnectWithType (connecting b) decideConnectType]) (layout
  a) (maxLabel a) (lengthXY a) (rightC a) (outerLayout a)
  where
    startLayer = findLayer (head (pointFrom $ connecting b)) (layered a) 0
    endLayer = findLayer (head (pointTo $ connecting b)) (layered a) 0
    stateToModify = if startLayer < endLayer then pointFrom $ connecting b else
      pointTo $ connecting b
    afterChange = changeRightConnections stateToModify (key a) (if connectType
      b == BackwardH then WithArrowhead else WithoutArrowhead) a -- changing right connection data
    deeperState = getDeeperState a stateToModify
    decideConnectType = if (connectType b == BackwardH) && (lengthXY deeperState > 2)
      then BackwardWH else connectType b

modifyRightConnection :: Wrapper -> Wrapper
modifyRightConnection s@OrDecom {} = OrDecom (fmap (fmap modifyRightConnection)
  (layered newOr)) (key s) (strings s) (connections newOr) (layout s)
  (maxLabel s) (lengthXY s) (rightC s) (outerLayout s)
  where
    dummyOr = OrDecom (layered s) (key s) (strings s) [] (layout s)
      (maxLabel s) (lengthXY s) (rightC s) (outerLayout s)
    newOr = foldl markRightConnection dummyOr (connections s)
modifyRightConnection s@AndDecom {} = AndDecom (fmap modifyRightConnection
  (component s)) (key s) (layout s) (lengthXY s) (rightC s)
  (outerLayout s)
modifyRightConnection s = s

changeLength :: Double -> Wrapper -> Wrapper
changeLength l s = case s of
  OrDecom {}
    | outerLayout s == Vertical -> OrDecom (layered s) (key s) (strings s)
        (connections s) (layout s) (maxLabel s) (l - h) (rightC s)
        (outerLayout s)
    | otherwise -> OrDecom (layered s) (key s) (strings s)
        (connections s) (layout s) (maxLabel s) (l - w) (rightC s)
        (outerLayout s)
  EndS {}
    | outerLayout s == Vertical ->
        EndS (key s) (l - h) (rightC s) (outerLayout s)
    | otherwise ->
        EndS (key s) (l - w) (rightC s) (outerLayout s)
  AndDecom {}
    | outerLayout s == Vertical -> AndDecom (component s) (key s)
        (layout s) (l - h) (rightC s) (outerLayout s)
    | otherwise -> AndDecom (component s) (key s)
        (layout s) (l - w) (rightC s) (outerLayout s)
  Leaf {}
    | outerLayout s == Vertical -> Leaf (key s) (strings s) (operation s)
        (l - h) (rightC s) (outerLayout s)
    | otherwise -> Leaf (key s) (strings s) (operation s)
        (l - w) (rightC s) (outerLayout s)
  Hist {}
    | outerLayout s == Vertical -> Hist (key s) (history s)
        (l - h) (rightC s) (outerLayout s)
    | otherwise -> Hist (key s) (history s) (l - w) (rightC s) (outerLayout s)
  CrossStateDummy {} -> s
  Dummy {}
    | outerLayout s == Vertical -> Dummy (key s) (outerLayout s) l
    | otherwise -> Dummy (key s) (outerLayout s) l
  Transition {}
    | outerLayout s == Vertical -> Transition (key s) (transitionName s)
        (l - h) (rightC s) (outerLayout s)
    | otherwise -> Transition (key s) (transitionName s)
        (l - w) (rightC s) (outerLayout s)
  Fork {}
    | outerLayout s == Vertical -> Fork (key s) (outerLayout s)
        (l - h) (rightC s)
    | otherwise -> Fork (key s) (outerLayout s) (l - w) (rightC s)
  StartS {}
    | outerLayout s == Vertical -> StartS (key s) (l - h) (rightC s)
        (outerLayout s)
    | otherwise -> StartS (key s) (l - w) (rightC s) (outerLayout s)
  where
    w = width (drawWrapper [] s)
    h = height (drawWrapper [] s)

assignLayerLength :: Layout -> [Wrapper] -> [Wrapper]
assignLayerLength layouts a = if layouts == Vertical then
  fmap (changeLength maxHeight) a else
  fmap (changeLength maxWidth) a
  where
    maxWidth = width (drawLayer True [] a)
    maxHeight = height (drawLayer False [] a)

assignLength :: Wrapper -> Wrapper
assignLength s@OrDecom {} = OrDecom newLayered (key s) (strings s)
      (connections s) (layout s) (maxLabel s) (lengthXY s) (rightC s
      ) (outerLayout s)
  where
    loop = OrDecom (fmap (fmap assignLength) (layered s)) (key s) (strings s)
      (connections s) (layout s) (maxLabel s) (lengthXY s) (rightC s
      ) (outerLayout s)
    newLayered = fmap (assignLayerLength (layout s)) (layered loop)
assignLength s@AndDecom {} = AndDecom (fmap assignLength (component s)) (key s)
  (layout s) (lengthXY s) (rightC s) (outerLayout s)
assignLength s = s

orderFunction :: UMLStateDiagram -> Wrapper
orderFunction a = loopEdgeRed 5 $
  modifyRightConnection $ assignLength $
  addDummy $
  reduceCrossStateCrossing' $ getWrapper $ rearrangeSubstate a

edgeCrossingReduc :: Wrapper -> Wrapper
edgeCrossingReduc s@OrDecom {} = case layered s of
  [[_]] -> OrDecom (fmap (fmap edgeCrossingReduc) (layered s)) (key s) (strings
    s) (connections s) (layout s) (maxLabel s) (lengthXY s) (rightC
    s) (outerLayout s)
  _ -> OrDecom withEdgeReduc (key s) (strings s) (connections s) (layout s)
    (maxLabel s) (lengthXY s) (rightC s) (outerLayout s)
    where
      loopDummy = fmap (fmap edgeCrossingReduc) (layered s)
      withEdgeReduc = edgeRedLayers [] loopDummy []
        (connectionsByLayers (connections s) loopDummy
        (buildEmptyConnectionByLayer loopDummy [])) 0 0 True

edgeCrossingReduc s@AndDecom {} = AndDecom (fmap edgeCrossingReduc (component
  s)) (key s) (layout s) (lengthXY s) (rightC s) (outerLayout s)
edgeCrossingReduc s = s

loopEdgeRed :: Int -> Wrapper -> Wrapper
loopEdgeRed 0 s = s
loopEdgeRed loop s = loopEdgeRed (loop - 1) (edgeCrossingReduc s)

edgeRedLayer :: [Wrapper] -> [Wrapper] -> [ConnectWithType] -> [Wrapper] ->
   Bool -> ([Wrapper], Int)
edgeRedLayer _ [] _ _ _ = ([], 0)
edgeRedLayer layerBef [a] connectionList layerAf checkType =
  (layerAf ++ [a], toAdd2 + toAdd)
  where
    toAdd = addCrossing layerBef (layerAf ++ [a]) connectionList checkType 0
    toAdd2 = addCrossing2 layerBef (layerAf ++ [a]) connectionList checkType 0
edgeRedLayer layerBef (a:b:xs) connectionList layerAf checkType
  | crossingNo1 > crossingNo2 = edgeRedLayer layerBef (a:xs) connectionList
      (layerAf ++ [b]) checkType
  | otherwise = edgeRedLayer layerBef (b:xs) connectionList (layerAf ++ [a])
      checkType
    where
      getListLayerBef1 = getConnectionWithLayerBefore2 a connectionList []
      getListLayerBef2 = getConnectionWithLayerBefore2 b connectionList []
      crossingNo1 = sum [higherIndex2 (x, y) layerBef checkType | x <-
        getListLayerBef1, y <- getListLayerBef2]
      crossingNo2 = sum [higherIndex2 (y, x) layerBef checkType | x <-
        getListLayerBef1, y <- getListLayerBef2]

edgeRedLayers :: [[Wrapper]] -> [[Wrapper]] -> [[Wrapper]] ->
  [[ConnectWithType]] -> Int -> Int -> Bool -> [[Wrapper]]
edgeRedLayers layersBefore [] layersAfter connectionList crossingBef
  crossingAfter loop
  | null layersBefore = edgeRedLayers layersAfter (reverse layersAfter) []
      (reverse connectionList) crossingAfter 0 (not loop) -- first run
  | crossingAfter < crossingBef = edgeRedLayers layersAfter (reverse
      layersAfter) [] (reverse connectionList) crossingAfter 0 (not loop)
  | otherwise = if loop then reverse layersBefore else layersBefore
edgeRedLayers layersBefore (x:xs) [] connectionList crossingBef crossingAfter
  loop = edgeRedLayers layersBefore xs [x] connectionList crossingBef
  crossingAfter loop
edgeRedLayers layersBefore (x:xs) layersAfter connectionList crossingBef
  crossingAfter loop = edgeRedLayers layersBefore xs (layersAfter ++ [fst
  modifiedLayer]) connectionList  crossingBef (crossingAfter + snd
  modifiedLayer) loop
  where
    wrapperToInt = last layersAfter
    modifiedLayer = edgeRedLayer wrapperToInt x (connectionList !! (length
      layersAfter - 1)) [] loop

countCrossStateCrossing :: [Int] -> [[Int]] -> [ConnectWithType] -> Int -> Int
countCrossStateCrossing _ [_] _ totalCrossing = totalCrossing
countCrossStateCrossing layerBef (a:b:xs) connectionList totalCrossing =
  countCrossStateCrossing layerBef (b:xs) connectionList (totalCrossing +
  crossingNo)
  where
    getListLayerBef1 = getConnectionWithLayerBefore3 a connectionList []
    getListLayerBef2 = getConnectionWithLayerBefore3 b connectionList []
    crossingNo = sum [higherIndex (x, y) layerBef | [x] <- getListLayerBef1,
      [y] <- getListLayerBef2]
countCrossStateCrossing _ _ _ _ = 0

addCrossing :: [Wrapper] -> [Wrapper] -> [ConnectWithType] -> Bool -> Int ->
  Int
addCrossing _ [] _ _ totalCrossing = totalCrossing
addCrossing layerBef (x:xs) connectionList checkType totalCrossing =
  case x of
    OrDecom {} -> addCrossing layerBef xs connectionList checkType
      (totalCrossing + addCrossingNo)
    AndDecom {} -> addCrossing layerBef xs connectionList checkType
      (totalCrossing + addCrossingNo)
    _ -> addCrossing layerBef xs connectionList checkType totalCrossing
  where
    compareList = getCompareList checkType x
    addCrossingNo = countCrossStateCrossing (fmap key layerBef) compareList
      connectionList 0

addCrossing2 :: [Wrapper] -> [Wrapper] -> [ConnectWithType] -> Bool -> Int -> Int
addCrossing2 _ [_] _ _ totalCrossing = totalCrossing
addCrossing2 layerBef (a:xs) connectionList checkType totalCrossing =
  addCrossing2 layerBef xs connectionList checkType (totalCrossing + newCrossing)
  where
    newCrossing = addCrossing2' layerBef a xs connectionList checkType 0
addCrossing2 _ _ _ _ _ = 0

addCrossing2' :: [Wrapper] -> Wrapper -> [Wrapper] -> [ConnectWithType] -> Bool -> Int -> Int
addCrossing2' _ _ [] _ _ totalCrossing = totalCrossing
addCrossing2' layerBef fixedWrapper (b:xs) connectionList checkType totalCrossing =
  addCrossing2' layerBef fixedWrapper xs connectionList checkType (totalCrossing + crossingNo)
  where
    getListLayerBef1 = getConnectionWithLayerBefore2 fixedWrapper connectionList []
    getListLayerBef2 = getConnectionWithLayerBefore2 b connectionList []
    crossingNo = sum [higherIndex2 (x, y) layerBef checkType | x <-
      getListLayerBef1, y <- getListLayerBef2]

--checkWrapper
checkWrapper :: UMLStateDiagram -> Maybe String
checkWrapper a
  | not(checkOuterMostWrapper b) = Just ("Error: Outermost layer must be 'OrDe"
    ++ "com' constructor")
  | not(checkOrDecomSubstate b) = Just ("Error: Substate of OrDecom constructo"
    ++ "r cannot be empty or just Hist/Fork/StartS/Dummy/Transition")
  | not(checkAndDecomSubstate b) = Just ("Error: AndDecom constructor must con"
    ++ "tain at least 2 OrDecom and no other type of constructor")
  | not(checkLayout b) = Just ("Error: Horizontal slicing must be followed by "
    ++ "vertical layering or vise versa")
  | otherwise = Nothing
    where
      b = addDummy $ getWrapper $ rearrangeSubstate a

checkOuterMostWrapper :: Wrapper -> Bool
checkOuterMostWrapper OrDecom {} = True
checkOuterMostWrapper AndDecom {} = True
checkOuterMostWrapper _ = False

checkOrDecomSubstate :: Wrapper -> Bool
checkOrDecomSubstate (AndDecom a _ _ _ _ _) = all checkOrDecomSubstate a
checkOrDecomSubstate (OrDecom a _ _ _ _ _ _ _ _) = any checkOrDecomList (concat a) &&
  all checkOrDecomSubstate (concat a)
checkOrDecomSubstate _ = True

checkOrDecomList :: Wrapper -> Bool
checkOrDecomList AndDecom {} = True
checkOrDecomList OrDecom {} = True
checkOrDecomList Leaf {} = True
checkOrDecomList EndS {} = True
checkOrDecomList _ = False

checkAndDecomSubstate :: Wrapper -> Bool
checkAndDecomSubstate (AndDecom a _ _ _ _ _) = length a > 1 && all checkAndDecomList a
checkAndDecomSubstate (OrDecom a _ _ _ _ _ _ _ _) = all checkAndDecomSubstate (concat a)
checkAndDecomSubstate _ = True

checkAndDecomList :: Wrapper -> Bool
checkAndDecomList (OrDecom a _ _ _ _ _ _ _ _) = all checkAndDecomSubstate (concat a)
checkAndDecomList _ = False

checkLayout :: Wrapper -> Bool
checkLayout a@(OrDecom [[b@AndDecom {}]] _ _ _ _ _ _ _ _) = layout a == layout b && checkLayout b
checkLayout (OrDecom a _ _ _ _ _ _ _ _) = all checkLayout (concat a)
checkLayout a@(AndDecom b _ _ _ _ _) = all (== layout a) (fmap layout b) && all checkLayout b
checkLayout _ = True
