module Generate (randomSD) where
import Datatype (
  Connection(..),
  HistoryType(..),
  StateDiagram(..),
  UMLStateDiagram,
  globalise
  )

import Checkers (checkSemantics, checkDrawability)
import Checkers.Helpers (checkEmptyOutTran,checkSameOutTran,inCompoundState, notHistory, isNotEnd, getSameFromTran, isSDCD, notJoint, globalStart,getAllElem1, lastSecNotCD, getSubstate, isNotCD)
import Data.Maybe(isNothing)
import Data.List((\\),nub,zip4) 
import Data.List.Extra(allSame)
import Test.QuickCheck hiding(label,labels)

-- define random Node value
data NodeType = Hist | End | Inner | Comb | Stat | Join  deriving Eq

-- the substate of SD must obey some rules 
checkSubType :: Int -> Bool -> Bool -> [NodeType] -> Bool
checkSubType _ _ _ [] = False
checkSubType subNum outermost leastTwoLevels x =
  length histNum < 2 && length endNum < 2 && length cdNum < 3 && length sdNum < 3 
  -- here limit the number of some ingredients to make the diagram not so complicated or sens
  && if outermost && leastTwoLevels then Comb `elem` x || Stat `elem` x
     else (End `elem` x || Inner `elem` x || Comb `elem` x || Stat `elem` x)  
  -- here satisfy the rule of (checkSUbstateSD in checkStructure ) 
  &&  if Comb `notElem` x && Stat `notElem` x then null joinNum else length joinNum < subNum - 1 
    where
      histNum = filter (== Hist) x
      endNum = filter (== End) x
      cdNum = filter (== Comb) x
      sdNum = filter (== Stat) x
      joinNum = filter (== Join) x

-- choose name fot each components in order to satify (checkSubNameUniq)
chooseName :: [NodeType] -> [String] -> [[String]]
chooseName (x:xs) str 
  = if x == Comb 
      then take 3 str : chooseName xs (drop 3 str) 
    else take 1 str : chooseName xs (drop 1 str)
chooseName [] _ = []

--to check if connection violate checkCrossings 
checkParallelRegion :: [Int] -> [Int] -> [UMLStateDiagram]-> Bool
checkParallelRegion [] _ _ = True
checkParallelRegion _ [] _ = True
checkParallelRegion [_] _ _ = True
checkParallelRegion _ [_] _ = True
checkParallelRegion [_,_] _ _ = True
checkParallelRegion _ [_,_] _ = True
checkParallelRegion (x:xs) (y:ys) subs 
  = case [x == y, all (isNotCD x) subs] of
     [True,True] -> checkParallelRegion xs ys (getSubstate x subs)
     [True,False] -> head xs == head ys && checkParallelRegion xs ys (getSubstate x subs)
     [False,_] -> True

--haveJoint:: UMLStateDiagram -> Bool
--haveJoint a = any (\x -> not (notJoint x (substate a))) (getAllElem a)  

suchThatWhileCounting :: Gen a -> (a -> Bool) -> Gen (a, Int)
suchThatWhileCounting gen p = tryWith 0
  where
    tryWith i = do
      a <- gen
      if p a then return (a, i) else tryWith (i + 1)

randomSD :: Gen (UMLStateDiagram, Int)
randomSD = do
  let outermost = True
      counter = 4 
      ns = [3 .. 4]
      alphabet = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y"]
      l = 1
      mustCD = False
      -- mustCD represents if a randoomSD' must have a CD
      cdMaxNum = 1 
      -- to ignore nested CD 
  nm <- elements alphabet
  leastTwoLevels <- frequency [(2,return False),(8,return True)]
  randomSD' outermost counter cdMaxNum leastTwoLevels ns alphabet (l,nm,mustCD) [] 
   `suchThatWhileCounting` (\sd -> isNothing (checkSemantics sd) && isNothing (checkDrawability sd))
  -- here outermost layer must be StateDigram (checkOutMostLayer) 
  
chooseRandomMustCD' :: NodeType -> Gen Bool
chooseRandomMustCD' Stat = choose (True,False)
chooseRandomMustCD' _ =  return False

randomSD' :: Bool -> Int -> Int -> Bool -> [Int] -> [String] -> (Int,String,Bool)-> [String] -> Gen UMLStateDiagram
randomSD' outermost c cdMaxNum leastTwoLevels ns alphabet (l,nm,mustCD) exclude = do
  n <- elements ns 
  -- number of substates
  labels <- shuffle  [1..(n+1)] 
  -- shuffle to achieve random assignment of label
  let counter = c - 1
      chooseNodeTypes = frequency [(1,return Hist),(1,return End),(5,return Inner),(1,return Comb),(1,return Stat),(10,return Join)]
      -- all types
      chooseNoCombJointNodeTypes = frequency [(1,return Hist),(1,return End),(5,return Inner),(1,return Stat)]
      chooseNoSubNodeTypes = frequency [(1,return Hist),(1,return End),(5,return Inner),(5,return Join)]
      -- types that have no substates 
      chooseNoSubJointNodeTypes = frequency [(1,return Hist),(1,return End),(5,return Inner)]
  subTypes1 <- vectorOf n (case (c > 0, cdMaxNum == 0) of
                            (True,True) -> chooseNoCombJointNodeTypes
                            (True,False) -> chooseNodeTypes 
                            (False,True) -> chooseNoSubJointNodeTypes
                            (False,False) -> chooseNoSubNodeTypes ) 
                              `suchThat` checkSubType n outermost leastTwoLevels 
  -- check counter > 0 to limit the depth the diagram
  let newMustCD = (Join `elem` subTypes1 && cdMaxNum /= 0) || mustCD
      subTypes = case (c > 0, newMustCD) of 
                  (True,True) ->  case (Comb `elem` subTypes1, Stat `elem` subTypes1) of 
                                    (True,_) -> subTypes1
                                    (False,True) -> subTypes1
                                    (False,False) -> subTypes1 ++ [Comb] 
                  (False,True) -> subTypes1 ++ [Comb]               
                  (_,False) -> subTypes1                              
  mustCDs <- case (c > 0, newMustCD) of 
              (True,True) ->  case (Comb `elem` subTypes1, Stat `elem` subTypes1) of 
                                (True,_) -> return (replicate n False)
                                (False,True) -> mapM chooseRandomMustCD' subTypes `suchThat` or
                                (False,False) -> return (replicate (n+1) False)
              (False,True) -> return (replicate (n+1) False)
              (_,False) -> return (replicate n False)
  let excludeNms   = exclude ++ [nm]
  -- record the StateDiagram used names that should not be used again in inner states 
  -- (checkNameUniqueness. checkSDNameUniq)
  subNms <- shuffle  (alphabet \\ excludeNms )
  let subNm   = chooseName subTypes subNms 
      cond    = zip4 labels subTypes subNm mustCDs
  -- (checkUniqueness (label))
  subs <- mapM (\x -> randomInnerSD counter cdMaxNum ns alphabet x excludeNms) cond  
  let layerElem = map (\x -> [label x]) subs
      innerElem = concatMap (getAllElem1 []) subs
      innerElemNoRegions = filter (`lastSecNotCD` subs) innerElem
      globalStartsWithoutCurrent = globalStart (StateDiagram subs l nm [] [])
      -- let there is no two startStates pointing the same node 
      -- and no StartState points to regions (checkStartToRegion)
  start <- 
    if outermost && Hist `elem` subTypes
      then elements (filter (not.(`notHistory` subs)) layerElem)
    else 
      if null innerElemNoRegions then elements layerElem 
      else frequency 
        [(2, elements (innerElemNoRegions \\ globalStartsWithoutCurrent))
         ,(8,elements layerElem)]
       -- let there is no two startStates pointing the same node 
  -- if there is an outermost History then start is this outermost History 
  let layerElemNoJoint = filter (`notJoint` subs) layerElem  
      innerElemNoRegionsJoint = filter (`notJoint` subs) innerElemNoRegions
  -- let Joint connections only be controlled at the outermost layer
  conns <-  vectorOf
    (length layerElemNoJoint)
    (randomConnection layerElemNoJoint innerElemNoRegionsJoint subs [])
    `suchThat`
      (\ x
         -> all (`checkSameOutTran` x) x && all (`checkEmptyOutTran` x) x)
  -- connections that from Joint/ to Joint is not considered
  let jointStates = filter (not.(`notJoint` subs)) (innerElem ++ layerElem)
      globalStarts  = globalStartsWithoutCurrent ++ [start]
  connsExtraJoint1 
    <- if outermost 
         then 
          mapM (randomJointConnection layerElemNoJoint innerElemNoRegionsJoint globalStarts subs) jointStates 
       else return []
  let connsExtraJoint = concat connsExtraJoint1
      innerElemNoRegionsJointSDCD = filter (not.(`isSDCD` subs)) innerElemNoRegionsJoint 
      globalConns  = connection (globalise (StateDiagram subs l nm (conns++ connsExtraJoint) []))
      toElem       = map pointTo globalConns
      reachabelStates = toElem ++ globalStarts
      unreachedStates = (layerElemNoJoint ++ innerElemNoRegionsJointSDCD) \\ reachabelStates
  connsExtra 
    <- (if outermost 
          then 
            mapM (randomConnection layerElemNoJoint innerElemNoRegionsJoint subs) unreachedStates
        else return [])
          `suchThat` 
            (\x -> all (`checkSameOutTran` x) x && all (`checkEmptyOutTran` x) x)
  return (StateDiagram subs l nm (conns ++ connsExtra ++ connsExtraJoint ) start)

randomInnerSD :: Int -> Int -> [Int] -> [String] -> (Int,NodeType,[String],Bool) -> [String]-> Gen UMLStateDiagram
randomInnerSD counter cdMaxNum ns alphabet (l,t,s,mustCD) exclude = do
  let nm = head s
  case t of 
       Hist  -> frequency [(1,return (History l Shallow)),(1,return (History l Deep))]
       End   -> return (EndState l)
       Inner -> return (InnerMostState l nm "")
       Comb -> randomCD counter cdMaxNum ns alphabet l s exclude
       Stat -> randomSD' False counter cdMaxNum False ns alphabet (l,nm,mustCD) exclude
       Join -> return (Joint l)

randomCD :: Int -> Int -> [Int]-> [String] -> Int -> [String] ->[String] -> Gen UMLStateDiagram
randomCD counter cdMaxNum ns alphabet l s exclude = do
  n      <- elements [2 .. 3]
  labels <- shuffle [1..n]
  let cdMaxNum' = cdMaxNum - 1
      mustCDs =replicate n False
      cond   = zip3 labels s mustCDs
  subs   <- mapM (\x -> randomSD' False counter cdMaxNum' False ns alphabet x exclude) cond
  return (CombineDiagram subs l)

randomConnection :: [[Int]] -> [[Int]] -> [UMLStateDiagram] -> [Int] -> Gen Connection
randomConnection layerElem innerElem sub unreachedState = do
  let points = layerElem ++ innerElem 
      endState  = filter (not.(`isNotEnd` sub)) points
      layerElemNoEnd = layerElem \\ endState 
      noEndState = points \\ endState  
      noParallelRegionFromElem1 = 
        filter (\x -> checkParallelRegion unreachedState x sub ) (noEndState \\ [unreachedState])
  from <- if null (layerElemNoEnd \\ [unreachedState]) then elements noParallelRegionFromElem1
          else 
            frequency [(3,elements noParallelRegionFromElem1),
                       (7,elements (layerElemNoEnd \\ [unreachedState]))]
  -- endStates have no outgoing edges (checkEndState)
  let outerHistory = filter (not.(`notHistory` sub)) layerElem
      layerElemNoOH = layerElem \\ outerHistory
      noOuterHistory = points \\ outerHistory
      transitionNms = ["a","b","c","d","e","f","g","h","i","j","k",""]
      excludeTranNms = concatMap (getSameFromTran from) sub
      fromSameNodeTranNms = (transitionNms \\ excludeTranNms) \\ [""]
      -- satisfy part of (checkSemantics) 
  let noParallelRegionToElem1 = filter (\x -> checkParallelRegion from x sub ) (noOuterHistory \\ [from])
  to <- if null unreachedState 
          then 
            if null noParallelRegionToElem1
              then elements (layerElemNoOH \\ [from])
            else 
              if null (layerElemNoOH \\ [from])
                then elements noParallelRegionToElem1
              else 
               frequency [(3,elements noParallelRegionToElem1),(7,elements (layerElemNoOH \\ [from]))]
        else return unreachedState
  tran <- elements (if not (null excludeTranNms) then fromSameNodeTranNms else transitionNms)
  let noParallelRegionFromElem2 = filter (\x -> checkParallelRegion to x sub ) (noEndState \\[to])
      noParallelRegionToElem2 = filter (\x -> checkParallelRegion from x sub ) (noOuterHistory \\ [from])
  case [notHistory from sub,notHistory to sub] of
    [True,True] -> return (Connection from to tran)
    [False,True] ->
                     if null unreachedState then do
                      historyTo <- elements (filter (inCompoundState from) noParallelRegionToElem2)
                      return (Connection from historyTo "")
                     else do
                      let onlyHistory = filter (not.(`notHistory` sub)) noParallelRegionFromElem1
                          validHistory = filter (`inCompoundState` to) onlyHistory
                          noEndHist = noParallelRegionFromElem1 \\ onlyHistory 
                      unreachedStateFrom <- elements (noEndHist ++ validHistory)
                      -- if to is the unreachedState ,from must be not a History that inside the unreachedState
                      return (Connection unreachedStateFrom to "")
    [_,False] -> do 
                  let onlyHistory = filter (not.(`notHistory` sub)) noParallelRegionFromElem2
                  historyFrom <- elements (filter (not . inCompoundState to) (noParallelRegionFromElem2 \\onlyHistory))
                  -- ignore the situation that when to is history and from is also history
                  if notHistory historyFrom sub then
                    return (Connection historyFrom to tran)
                  else
                    return (Connection historyFrom to "")

randomJointConnection :: [[Int]] -> [[Int]] -> [[Int]] -> [UMLStateDiagram] -> [Int] -> Gen [Connection]
randomJointConnection layerElem innerElem globalStarts subs joint = do 
    fromNum <- if joint `elem` globalStarts then return 2 else choose (1,2)
    -- if condition satisfy rules when the start node pointing to the joint 
    let points = layerElem ++ innerElem
        noHistory = filter (`notHistory` subs) points
        -- here let Joint will not point to/from History
       -- noParallelRegionHistoryElem = filter (\x -> checkParallelRegion joint x subs) noHistory
    cd <- elements (filter (not.(`notCD` subs)) noHistory)  
    let cdSub = filter (\ x -> take (length cd) x == cd) noHistory \\ [cd]
    jointOut <- vectorOf fromNum (elements (if fromNum == 1 then noHistory \\[joint] else cdSub \\[joint]))
                  `suchThat` (\x -> length (nub x) == length x && checkDistinctRegion cd x )
    let transitionNms = ["a","b","c","d","e","f","g","h","i","j","k",""]
    jointOutNm <- if joint `elem` globalStarts then return "" else elements transitionNms
    let jointOutConn = map (\x -> Connection joint x jointOutNm) jointOut
    if joint `elem` globalStarts
      then 
        return jointOutConn
    else do
      let toNum = if fromNum == 1 then 2 else 1 
          noEndState  = filter (`isNotEnd` subs) noHistory
          cdSubsNoEndState = filter (`isNotEnd` subs) cdSub
      jointIn <- vectorOf toNum (elements (if toNum == 1 then noEndState\\[joint] else cdSubsNoEndState \\[joint] ))
                     `suchThat` (\x -> length (nub x) == length x && checkDistinctRegion cd x )
      jointInNm <- if jointOutNm == "" then elements (transitionNms \\ [""]) else return ""
      let jointInConn = map (\x -> Connection x joint jointInNm) jointIn
      return (jointOutConn ++ jointInConn)

checkDistinctRegion :: [Int] -> [[Int]] -> Bool
checkDistinctRegion _ [_] = True
checkDistinctRegion cd points 
  = not (allSame headOfPointsInsideCD)
    where 
       headOfPointsInsideCD = map (last . take (length cd + 1)) points

notCD :: [Int] -> [UMLStateDiagram] -> Bool
notCD [] _ = True
notCD [x] a = all (isNotCD x) a
notCD (x:xs) a = notCD xs (getSubstate x a)

