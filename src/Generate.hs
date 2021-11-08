module Generate (randomSD) where
import Datatype (
  Connection(..),
  HistoryType(..),
  StateDiagram(..),
  UMLStateDiagram,
  globalise
  )
import Test (checkSemantics,checkJoint,checkCrossings)
import Helper
import Data.Maybe(isNothing)
import Data.List((\\),nub) 
import Test.QuickCheck hiding(label,labels)

data NodeType = Hist | End | Inner | Comb | Stat | Join  deriving Eq

chooseType :: [NodeType] -> Gen NodeType 
chooseType t = 
  if length t == 6 
    then 
      frequency [(1,return Hist),(1,return End),(5,return Inner),(1,return Comb),(1,return Stat),(1,return Join)]
  else 
    frequency [(1,return Hist),(1,return End),(5,return Inner),(1,return Join)]  

suchThatWhileCounting :: Gen a -> (a -> Bool) -> Gen (a, Int)
suchThatWhileCounting gen p = tryWith 0
  where
    tryWith i = do
      a <- gen
      if p a then return (a, i) else tryWith (i + 1)

randomSD :: Gen (UMLStateDiagram, Int)
randomSD = do
      let alphabet = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y"]
      nm <- elements alphabet
      randomSD' 4 4 [3 .. 4] alphabet (1,nm) [] `suchThatWhileCounting` 
                  (\x -> isNothing (checkSemantics x) && isNothing (checkJoint x) && isNothing (checkCrossings x))

randomSD' :: Int -> Int -> [Int] -> [String] -> (Int,String)-> [String] -> Gen UMLStateDiagram
randomSD' depth c ns alphabet (l,nm) exclude = do
  let counter = c - 1
      noNms   = exclude ++ [nm]
      nodeTypes = [Hist,End,Inner,Comb,Stat,Join]
      noSubNodeTypes = [Hist,End,Inner,Join]
  n      <- elements ns
  labels <- shuffle  [1..n]
  subTypes <- vectorOf n (if counter > 0 then chooseType nodeTypes else chooseType noSubNodeTypes) `suchThat` checkSubType
  subNms <- shuffle  (alphabet \\ noNms)
  let subNm   = chooseName subTypes subNms 
      cond    = zip3 labels subTypes subNm 
  subs <- mapM (\x -> randomInnerSD depth counter ns alphabet x noNms) cond
  let layerElem  = map (\x -> [label x]) subs
      innerElem  = concatMap (getAllElem1 []) subs
      innerElemNoRegions = filter (`lastSecNotCD` subs) innerElem
  start <- elements (if depth == c && Hist `elem` subTypes
                    then filter (not.(`notHistory` subs)) layerElem
                    else filter (`lastSecNotCD` subs) (layerElem ++ innerElemNoRegions))
                    -- outermost History will impact on REACHABILITY (Only outCompount ingoing edge) 
  conns <- vectorOf n 
                 (randomConnection (layerElem ++ innerElemNoRegions) subs start [])

  let innerElemOnlySDCD = filter (`isSDCD` subs) innerElem 
      innerElemNotSDCD = innerElemNoRegions \\ innerElemOnlySDCD
      globalStarts  = nub (globalStart (StateDiagram subs l nm [] start ))
      globalConns   = connection (globalise (StateDiagram subs l nm conns []))
      toElem       = nub (map pointTo globalConns)
      reachabelStates = toElem ++ globalStarts 
      unreachedStates = (layerElem ++ innerElemNotSDCD) \\ reachabelStates         
  if depth == c && not (null  unreachedStates)
    then do
      connsExtra <- mapM (randomConnection (layerElem ++ innerElemNoRegions) subs start) unreachedStates
      return (StateDiagram subs l nm (conns ++ connsExtra) start)
  else 
    return (StateDiagram subs l nm conns start)

checkSubType :: [NodeType] -> Bool
checkSubType [] = False
checkSubType a  = length histNum < 2 && length endNum < 2 && length cdNum < 2 && length sdNum < 3
                  && ( End `elem` a || Inner `elem` a || Comb `elem` a || Stat `elem` a )
                  where
                    histNum = filter (== Hist) a
                    endNum = filter (== End) a
                    cdNum = filter (== Comb) a
                    sdNum = filter (== Stat) a

chooseName :: [NodeType] -> [String] -> [[String]]
chooseName (x:xs) str = if x == Comb 
                          then 
                             take 3 str : chooseName xs (drop 3 str) 
                        else 
                           take 1 str : chooseName xs (drop 1 str)
chooseName [] _ = []

randomInnerSD :: Int -> Int -> [Int] -> [String] -> (Int,NodeType,[String]) -> [String]-> Gen UMLStateDiagram
randomInnerSD depth counter ns alphabet (l,t,s) exclude = do
  let nm = head s
  case t of 
       Hist  -> frequency [(1,return (History l Shallow)),(1,return (History l Deep))]
       End   -> return (EndState l)
       Inner -> return (InnerMostState l nm "")
       Comb -> randomCD depth counter ns alphabet l s exclude
       Stat -> randomSD' depth counter ns alphabet (l,nm) exclude
       Join -> return (Joint l)

randomCD :: Int -> Int -> [Int]-> [String] -> Int -> [String] ->[String] -> Gen UMLStateDiagram
randomCD depth c ns alphabet l s exclude = do
      let counter = c - 1
      n      <- elements [2 .. 3]
      labels <- shuffle [1..n]
      let cond   = zip labels s
      subs   <- mapM (\x -> randomSD' depth counter ns alphabet x exclude) cond
      return (CombineDiagram subs l)

randomConnection ::[[Int]] -> [UMLStateDiagram] -> [Int] -> [Int] -> Gen Connection
randomConnection points sub start unreachedState = do
  let endState  = filter (not.(`isNotEnd` sub)) points
      outerHistory = filter (\x -> length x == 1 ) onlyHistory  
                      where  
                       onlyHistory = filter (not.(`notHistory` sub)) points 
      histState = filter (not.(`notHistory` sub)) points
      noEndState = points \\ endState 
      noEndHist = noEndState \\histState
      noOuterHistory = points \\ outerHistory
      noOutHistHist = noOuterHistory \\ histState
      transitionNms = ["a","b","c","d","e","f","g","h","i","j","k",""]
  from <- elements (noEndState \\ [unreachedState])
  let noTranNms = concatMap (getSameFromTran from) sub
  if length noTranNms < 4 
    -- let the number of edges from the same node limit to 4
    then 
      do
        to   <- elements (if all null [unreachedState] then noOuterHistory \\ [from] else [unreachedState]) 
        tran <- elements (if not (notJoint start sub) && start == from then transitionNms \\ [""] else transitionNms)
        case [notHistory from sub,notHistory to sub,notJoint from sub] of 
          [True,True,True] -> do
                           tr <- elements (if all (checkMoreOut from) sub
                                                then transitionNms \\ noTranNms
                                           else (transitionNms \\ noTranNms) \\ [""])
                           return (Connection from to tr)
          [True,True,False] -> do 
                            return (Connection from to tran)
          [True,False,True] -> do
                           historyFrom <- elements (filter (not . inCompoundState to) (noEndHist\\[to])) 
                           tr <- elements (if all (checkMoreOut historyFrom) sub
                                                then transitionNms \\ noTranNms
                                           else (transitionNms \\ noTranNms) \\ [""] )
                           return (Connection historyFrom to tr)
          [True,False,False] -> do
                           historyFrom <- elements (filter (not . inCompoundState to) (noEndHist\\[to])) 
                           return (Connection historyFrom to tran)
          [False,True,True] -> do
                           historyTo <- elements (filter (inCompoundState from) (noOutHistHist\\[from]))
                           return (Connection from historyTo "")
          [False,True,False] -> do -- not valid
                           historyTo <- elements (filter (inCompoundState from) (noOutHistHist\\[from]))
                           return (Connection from historyTo "")
          [False,False,True] -> do
                           historyFrom <- elements (filter (not . inCompoundState to) (noEndHist\\[to]))
                           tr <- elements (if all (checkMoreOut historyFrom) sub
                                                then transitionNms \\ noTranNms
                                           else  (transitionNms \\ noTranNms) \\ [""])
                           return (Connection historyFrom to tr)
          [False,False,False] -> do -- not valid 
                           historyFrom <- elements (filter (not . inCompoundState to) (noEndHist\\[to]))
                           return (Connection historyFrom to tran)
  else 
    randomConnection (points\\[from]) sub start unreachedState