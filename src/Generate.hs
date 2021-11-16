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

-- define random Node value
data NodeType = Hist | End | Inner | Comb | Stat | Join  deriving Eq

-- the substate of SD must obey some rules 
checkSubType :: Int -> [NodeType] -> Bool
checkSubType _ [] = False
checkSubType subNum x =
  length histNum < 2 && length endNum < 2 && length cdNum < 2 && length sdNum < 3 
  -- here limit the number of some ingredients to make the diagram not so complicated or sens
  && (End `elem` x || Inner `elem` x || Comb `elem` x || Stat `elem` x) 
  -- here satisfy the rule of (checkSUbstateSD in checkStructure ) 
  &&  if Comb `notElem` x && Stat `notElem` x then null joinNum else length joinNum < subNum - 1 
  -- to limit the number of joint to zero when it goes to innermost layer
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
--checkParallelRegion :: [Int] -> [Int] -> [UMLStateDiagram]-> Bool
--checkParallelRegion [] _ _ = True
--checkParallelRegion _ [] _ = True
--checkParallelRegion [_] _ _ = True
--checkParallelRegion _ [_] _ = True
--checkParallelRegion [_,_] _ _= True
--checkParallelRegion _ [_,_] _= True
--checkParallelRegion a@[x,xs,_] [y,ys,_] subs 
--  = if x == y then not (not (lastThirdNotCD a subs) && xs /= ys) else True 
--checkParallelRegion (x:xs) (y:ys) subs 
--  = if x == y then checkParallelRegion xs ys (getSubstate x subs) else True 

--lastThirdNotCD :: [Int] -> [UMLStateDiagram]-> Bool
--lastThirdNotCD [] _ = True
--lastThirdNotCD [x,_,_] a = all (lastThirdNotCD1 x) a
--lastThirdNotCD (x:xs) a = lastThirdNotCD xs (getSubstate x a) 

--lastThirdNotCD1 :: Int -> UMLStateDiagram -> Bool
--lastThirdNotCD1 a CombineDiagram{label} = a /= label
--lastThirdNotCD1 _ _ = True

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
  randomSD' True 4 [3 .. 4] alphabet (1,nm) [] `suchThatWhileCounting` 
    (\x -> isNothing (checkSemantics x) && isNothing (checkCrossings x ) && isNothing (checkJoint x ))
  -- here outermost layer must be StateDigram (checkOutMostLayer) --&& isNothing (checkJoint x)

randomSD' :: Bool -> Int -> [Int] -> [String] -> (Int,String)-> [String] -> Gen UMLStateDiagram
randomSD' outermost c ns alphabet (l,nm) exclude = do
  n <- elements ns 
  -- number of substates
  labels <- shuffle  [1..n] 
  -- shuffle to achieve random assignment of label
  let counter = c - 1
      chooseNodeTypes = frequency [(1,return Hist),(1,return End),(5,return Inner),(1,return Comb),(2,return Stat),(1,return Join)]
      -- all types
      chooseNoSubNodeTypes = frequency [(1,return Hist),(1,return End),(5,return Inner),(1,return Join)]
      -- types that have no substates 
  subTypes <- vectorOf n (if counter > 0 then chooseNodeTypes else chooseNoSubNodeTypes)
                        `suchThat` checkSubType n
  -- check counter > 0 to limit the depth the diagram
  let excludeNms   = exclude ++ [nm]
  -- record the StateDiagram used names that should not be used again in inner states 
  -- (checkNameUniqueness. checkSDNameUniq)
  subNms <- shuffle  (alphabet \\ excludeNms )
  let subNm   = chooseName subTypes subNms 
      cond    = zip3 labels subTypes subNm 
  -- (checkUniqueness (label))
  subs <- mapM (\x -> randomInnerSD False counter ns alphabet x excludeNms) cond
  let layerElem = map (\x -> [label x]) subs
      innerElem = concatMap (getAllElem1 []) subs
      innerElemNoRegions = filter (`lastSecNotCD` subs) innerElem
      globalStartsWithoutCurrent = globalStart (StateDiagram subs l nm [] [])
      startchoice = (layerElem ++ innerElemNoRegions) \\ globalStartsWithoutCurrent
      -- let there is no two startStates pointing the same node 
      -- and no StartState points to regions (checkStartToRegion)
  start <- elements (if outermost && Hist `elem` subTypes
                       then filter (not.(`notHistory` subs)) layerElem
                     else startchoice )
  -- if there is an outermost History then start is this outermost History 
  let layerElemNoJoint = filter (`notJoint` subs) layerElem  
      innerElemNoRegionsJoint = filter (`notJoint` subs) innerElemNoRegions
  -- let Joint connections only be controlled at the outermost layer
  conns <- vectorOf (n - 1)
            (randomConnection layerElemNoJoint innerElemNoRegionsJoint subs [])
  -- connections that from Joint/ to Joint is not considered
-- could add check same connection ???
  let jointStates = filter (not.(`notJoint` subs)) (innerElem ++ layerElem)
      globalStarts  = globalStartsWithoutCurrent ++ [start]
  connsExtraJoint1 
    <- if outermost 
         then 
          mapM (randomJointConnection layerElem innerElemNoRegions globalStarts subs) jointStates 
       else return []
  let connsExtraJoint = concat connsExtraJoint1
      innerElemNoRegionsJointSDCD = filter (not.(`isSDCD` subs)) innerElemNoRegionsJoint 
      globalConns  = connection (globalise (StateDiagram subs l nm (conns++ connsExtraJoint) []))
      toElem       = map pointTo globalConns
      reachabelStates = toElem ++ globalStarts
      unreachedStates = (layerElemNoJoint ++ innerElemNoRegionsJointSDCD) \\ reachabelStates
  connsExtra 
    <- if outermost 
          then 
            mapM (randomConnection layerElemNoJoint innerElemNoRegionsJoint subs) unreachedStates
        else return []
  return (StateDiagram subs l nm (conns ++ connsExtra ++ connsExtraJoint ) start)

                                                                                                                                                                               
randomInnerSD :: Bool -> Int -> [Int] -> [String] -> (Int,NodeType,[String]) -> [String]-> Gen UMLStateDiagram
randomInnerSD outermost counter ns alphabet (l,t,s) exclude = do
  let nm = head s
  case t of 
       Hist  -> frequency [(1,return (History l Shallow)),(1,return (History l Deep))]
       End   -> return (EndState l)
       Inner -> return (InnerMostState l nm "")
       Comb -> randomCD outermost counter ns alphabet l s exclude
       Stat -> randomSD' outermost counter ns alphabet (l,nm) exclude
       Join -> return (Joint l)

randomCD :: Bool -> Int -> [Int]-> [String] -> Int -> [String] ->[String] -> Gen UMLStateDiagram
randomCD outermost c ns alphabet l s exclude = do
  let counter = c - 1
  n      <- elements [2 .. 3]
  labels <- shuffle [1..n]
  let cond   = zip labels s
  subs   <- mapM (\x -> randomSD' outermost counter ns alphabet x exclude) cond
  return (CombineDiagram subs l)

randomConnection :: [[Int]] -> [[Int]] -> [UMLStateDiagram] -> [Int] -> Gen Connection
randomConnection layerElem innerElem sub unreachedState = do
  let points = layerElem ++ innerElem 
      endState  = filter (not.(`isNotEnd` sub)) points
      noEndState = points \\ endState 
      --noParallelRegionFromElem1 = filter (\x -> checkParallelRegion unreachedState x sub ) (noEndState \\ [unreachedState])
  from <- elements (noEndState \\ [unreachedState])-- (seq hw noParallelRegionFromElem1)
  -- endStates have no outgoing edges (checkEndState)
  let outerHistory = filter (not.(`notHistory` sub)) layerElem
      noOuterHistory = points \\ outerHistory
      transitionNms = ["a","b","c","d","e","f","g","h","i","j","k",""]
      excludeTranNms = concatMap (getSameFromTran from) sub
      fromSameNodeTranNms = (transitionNms \\ excludeTranNms) \\ [""]
      -- satisfy part of (checkSemantics) 
  --let noParallelRegionToElem1 = filter (\x -> checkParallelRegion from x sub ) (noOuterHistory \\ [from])
  --hw <- if null (noParallelRegionToElem1) then error ("endState: " ++ show endState  
  --                                                            ++  "points:" ++ show points
  --                                                           ++  "unreachedState:" ++ show unreachedState
  --                                                            ++ "from" ++ show from 
  --                                                            ++ "subs" ++ show sub
  --                                                            ++ show depth ++ show c) else return ()
  to   <- elements (if null unreachedState then noOuterHistory \\ [from] else [unreachedState])
  tran <- elements (if not (null excludeTranNms) then fromSameNodeTranNms else transitionNms)
  --let noParallelRegionFromElem2 = filter (\x -> checkParallelRegion to x sub ) (noEndState \\[to])
   --   noParallelRegionToElem2 = filter (\x -> checkParallelRegion from x sub ) (noOuterHistory \\ [from])
  case [notHistory from sub,notHistory to sub] of 
    [True,True] -> do
                     return (Connection from to tran)
    [True,False] -> do
                    -- hc <- if null noParallelRegionFromElem2 then error "44444" else return ()
                     --hd <- if null (filter (not . inCompoundState to) noParallelRegionFromElem2)  then error "55555" else return ()
                     historyFrom <- elements (filter (not . inCompoundState to) (noEndState \\[to])) 
                     if notHistory historyFrom sub then do 
                      return (Connection historyFrom to tran)
                     else do 
                      return (Connection historyFrom to "")
    [False,True] -> do
                     if null unreachedState then do 
                    --  hc <- if null noParallelRegionToElem2 then error "66666" else return () 
                     -- hd <- if null (filter (inCompoundState from) noParallelRegionToElem2)  then error "777777" else return ()
                      historyTo <- elements (filter (inCompoundState from) (noOuterHistory \\ [from]))
                      return (Connection from historyTo "")
                     else do
                      let onlyHistory = filter (not.(`notHistory` sub)) noEndState
                          validHistory = filter (`inCompoundState` to) onlyHistory
                          noEndHist = noEndState \\ onlyHistory 
                      unreachedStateFrom <- elements (noEndHist ++ validHistory)
                      -- if to is the unreachedState ,from must be not a History that inside the unreachedState
                      return (Connection unreachedStateFrom to "")
    [False,False] -> do
                     historyFrom <- elements (filter (not . inCompoundState to) (noEndState \\[to])) 
                     if notHistory historyFrom sub then do 
                      return (Connection historyFrom to tran)
                     else do 
                      return (Connection historyFrom to "")

randomJointConnection :: [[Int]] -> [[Int]] -> [[Int]] -> [UMLStateDiagram] -> [Int] -> Gen [Connection]
randomJointConnection layerElem innerElem globalStarts subs joint = do 
  if joint `elem` globalStarts 
    then do 
      fromNum <- elements [2..3]
      let points = layerElem ++ innerElem 
          outerHistory = filter (not.(`notHistory` subs)) layerElem
          noOuterHistory = points \\ outerHistory 
      jointOut <- vectorOf fromNum (elements (noOuterHistory \\[joint])) 
                    `suchThat` (\x -> length (nub x) == length x  
                         && all (\xs -> notHistory xs subs || not (inCompoundState xs joint)) x) 
      let jointOutConn = map (\x -> Connection joint x "") jointOut
      return jointOutConn
  -- satisfy rules when the start node pointing to the joint 
  else do
    n <- elements [2..3]
    fromNum <- choose (1,n)
    let points = layerElem ++ innerElem 
        outerHistory = filter (not.(`notHistory` subs)) layerElem
        noOuterHistory = points \\ outerHistory 
    jointOut <- vectorOf fromNum (elements (noOuterHistory \\[joint])) 
                  `suchThat` (\x -> length (nub x) == length x 
                    && all (\xs -> notHistory xs subs || not (inCompoundState xs joint)) x)
    let transitionNms = ["a","b","c","d","e","f","g","h","i","j","k",""]
    jointOutNm <- elements transitionNms
    let jointOutConn = map (\x -> Connection joint x jointOutNm) jointOut
        toNum = if fromNum == 1 then n else 1 
        endState  = filter (not.(`isNotEnd` subs)) points
        noEndState = points \\ endState 
    jointIn <- vectorOf toNum (elements (noEndState\\[joint])) 
                   `suchThat` (\x -> length (nub x) == length x 
                     && all (\xs -> notHistory xs subs || (jointOutNm /= "" && inCompoundState xs joint)) x )
    jointInNm <- if jointOutNm == "" then elements (transitionNms \\ [""]) else return ""
    let jointInConn = map (\x -> Connection x joint jointInNm) jointIn
    return (jointOutConn ++ jointInConn)
