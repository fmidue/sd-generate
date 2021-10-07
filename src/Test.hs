{-# LANGUAGE NamedFieldPuns #-}
module Test
  ( checkSemantics
  , checkStartState
  , checkEndState
  , checkConnection
  , checkCrossings
  , checkNameUniqueness
  , checkUniqueness
  , checkStructure
  , checkJoint
  ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram,
  globalise,
  localise,
  )

import Data.List.Extra


--checkEndState
checkEndState :: UMLStateDiagram -> Maybe String
checkEndState a
  | not(checkEndOutEdges a) = Just "Error: no EndState should have outgoing edges"
  | otherwise = Nothing

checkEndOutEdges :: UMLStateDiagram -> Bool
checkEndOutEdges StateDiagram { substate, connection } = all ((`isConnFromNotEnd` substate) . pointFrom) connection
                                                         && all checkEndOutEdges substate
checkEndOutEdges CombineDiagram {substate} = all checkEndOutEdges substate
checkEndOutEdges  _ = True

isConnFromNotEnd :: [Int] -> [UMLStateDiagram] -> Bool
isConnFromNotEnd [] _ = True
isConnFromNotEnd [x] a = all (isNotEnd x) a
isConnFromNotEnd (x:xs) a = isConnFromNotEnd xs (getSubstate x a)

isNotEnd :: Int -> UMLStateDiagram -> Bool
isNotEnd a EndState {label}  = a /= label
isNotEnd _ _ = True

--checkStartState
checkStartState :: UMLStateDiagram -> Maybe String
checkStartState a
  | not(checkSubS a) = Just "Error: invalid start state "
  | not(checkStartToRegion a) = Just "Start to regions themselves"
  | otherwise = Nothing

checkSubS :: UMLStateDiagram -> Bool
checkSubS  StateDiagram { substate, startState} = checkStart && all checkSubS substate
                                              where
                                                getLayerList = map label substate
                                                checkStart = isContained1 startState getLayerList substate
checkSubS CombineDiagram {substate} = all checkSubS substate
checkSubS  _ = True

checkStartToRegion :: UMLStateDiagram -> Bool
checkStartToRegion StateDiagram{substate,startState}  = 
                                               lastSecCD startState substate 
                                             && all checkStartToRegion substate
checkStartToRegion _ = True                                          

--check Connection Points
checkConnection :: UMLStateDiagram -> Maybe String
checkConnection a
  | not(checkSubC a) = Just "Error: Connection Points"
  | not(checkConnFromToRegion a) = Just "connections from/to regions themselves "
  | otherwise = Nothing

checkSubC :: UMLStateDiagram -> Bool
checkSubC  StateDiagram { substate, connection } =  checkConnFrom && checkConnTo  && all checkSubC substate
                              where
                                getLayerList = map label substate
                                checkConnFrom = isContained (map pointFrom connection) getLayerList  substate
                                checkConnTo = isContained (map pointTo connection ) getLayerList substate
checkSubC CombineDiagram {substate} = all checkSubC substate
checkSubC  _ = True

isContained :: [[Int]] -> [Int] -> [UMLStateDiagram] -> Bool
isContained xs a b = all (\x -> isContained1 x a b) xs

isContained1 :: [Int] -> [Int] -> [UMLStateDiagram] -> Bool
isContained1 [] _ _ = True
isContained1 (x:xs) a b =  (x `elem` a)  &&
       isContained1 xs (map label (getSubstate x b)) (getSubstate x b)

getSubstate :: Int -> [UMLStateDiagram] -> [UMLStateDiagram]
getSubstate a xs = maybe [] getSubstate1 (find ((a ==) . label) xs)

getSubstate1 :: UMLStateDiagram -> [UMLStateDiagram]
getSubstate1 (StateDiagram a _ _ _ _) = a
getSubstate1 (CombineDiagram a _) = a
getSubstate1 _ = []

checkConnFromToRegion :: UMLStateDiagram -> Bool
checkConnFromToRegion a  = 
                      all (\cs ->  all ((`lastSecCD` sub) . pointFrom) cs
                                && all ((`lastSecCD` sub) . pointTo)   cs ) a
                      where sub = substate a

lastSecCD :: [Int] -> [UMLStateDiagram]-> Bool
lastSecCD [] _ = True
lastSecCD [x, _] a = all (isNotCD x) a
lastSecCD (x:xs) a = lastSecCD xs (getSubstate x a) 

isNotCD :: Int -> UMLStateDiagram -> Bool
isNotCD a CombineDiagram{label} = a /= label
isNotCD _ _ = True

checkCrossings :: UMLStateDiagram -> Maybe String
checkCrossings s = case connections s - connections (localise s) of
  0 -> Nothing
  n -> Just $ "Has " ++ show n ++ " illegal crossing(s) between regions"
  where
    connections = foldr ((+) . length) 0

--check name uniqueness
checkNameUniqueness :: UMLStateDiagram -> Maybe String
checkNameUniqueness a
  | not(checkSubNameUniq a) = Just "Error: At the same layer name Uniqueness is not fullfilled "
  | not(checkSDNameUniq a) = Just ("Error: In each StateDiagram, the name (if not empty) should be different"
    ++"from all names found arbitrarily deep inside the substates.")
  | otherwise = Nothing

checkSDNameUniq :: UMLStateDiagram -> Bool
checkSDNameUniq StateDiagram {substate,name} = name `notElem` getLayerName substate
                                              && all (checkDeeperUniq name) substate
                                              && all checkSDNameUniq substate
checkSDNameUniq CombineDiagram {substate} = all checkSDNameUniq substate
checkSDNameUniq  _ = True

checkDeeperUniq :: String -> UMLStateDiagram -> Bool
checkDeeperUniq a StateDiagram {substate} = a `notElem` getLayerName substate
                                              && all (checkDeeperUniq a) substate
checkDeeperUniq a CombineDiagram {substate} = all (checkDeeperUniq a) substate
checkDeeperUniq _ _ = True

checkSubNameUniq :: UMLStateDiagram -> Bool
checkSubNameUniq StateDiagram {substate} =  not (anySame (getLayerName substate))
                                            && all checkSubNameUniq  substate
checkSubNameUniq  CombineDiagram {substate} = not (anySame (getLayerName substate))
                                              && all checkSubNameUniq  substate
checkSubNameUniq  _ = True

getLayerName :: [UMLStateDiagram] -> [String]
getLayerName a = filter (not . null) (concatMap getName a)

getName :: UMLStateDiagram -> [String]
getName StateDiagram{name} = [name]
getName InnerMostState{name} = [name]
getName CombineDiagram{substate} = concatMap getName substate
getName _ = []

--check local uniqueness
checkUniqueness :: UMLStateDiagram -> Maybe String
checkUniqueness a
  | not(checkSub a) = Just "Error: Local Uniqueness not fullfilled "
  | otherwise = Nothing

checkSub :: UMLStateDiagram -> Bool
checkSub  StateDiagram {substate} =  isUnique (map label substate ) && all checkSub substate
checkSub  CombineDiagram {substate} =  isUnique (map label substate ) && all checkSub substate
checkSub  _ = True

isUnique :: [Int] -> Bool
isUnique a = not (anySame a)

--checkStructure
checkStructure :: UMLStateDiagram -> Maybe String
checkStructure a
  | not(checkOuterMostLayer a) = Just ("Error: Outermost layer must be 'StateD"
    ++ "iagram")
  | not(checkSubstateSD a) = Just ("Error: Substate of StateDiagram constructo"
    ++ "r cannot be empty or just History/Joint")
  | not(checkSubstateCD a) = Just ("Error: CombineDiagram constructor must con"
    ++ "tain at least 2 StateDiagram and no other type of constructor")
  | not(checkHistOutTransition a) = Just ("Error: Outgoing edges from a history"
    ++ "node always have the empty transition")
  | not(checkEmptyConnPoint a) = Just ("Error: Neither the pointFrom nor the po" 
    ++ "intTo of a connection is an empty list")
  | otherwise = Nothing

                --checkOuterMostLayer
checkOuterMostLayer :: UMLStateDiagram -> Bool
checkOuterMostLayer StateDiagram{} = True
checkOuterMostLayer _ = False

                --checkSubstateSD
checkSubstateSD :: UMLStateDiagram -> Bool
checkSubstateSD (CombineDiagram a _) = all checkSubstateSD a
checkSubstateSD (StateDiagram a _ _ _ _) = any checkListInSD a && all checkSubstateSD a
checkSubstateSD _ = True

checkListInSD :: UMLStateDiagram -> Bool
checkListInSD Joint {} = False
checkListInSD History {} = False
checkListInSD _ = True

                --checkSubstateCD
checkSubstateCD :: UMLStateDiagram -> Bool
checkSubstateCD (CombineDiagram a _) = length a > 1 && all checkListInCD a
checkSubstateCD (StateDiagram a _ _ _ _) = all checkSubstateCD a
checkSubstateCD _ = True

checkListInCD :: UMLStateDiagram -> Bool
checkListInCD (StateDiagram a _ _ _ _) = all checkSubstateCD a
checkListInCD _ = False

                 --checkHistOutTransitions
checkHistOutTransition :: UMLStateDiagram -> Bool
checkHistOutTransition StateDiagram { substate, connection } = all (`checkHistConnTransition` substate) connection
                                                               && all checkHistOutTransition substate
checkHistOutTransition CombineDiagram {substate} = all checkHistOutTransition substate
checkHistOutTransition  _ = True

checkHistConnTransition :: Connection -> [UMLStateDiagram] -> Bool
checkHistConnTransition Connection { pointFrom,transition } a = isConnFromNotHistory pointFrom a || null transition

isConnFromNotHistory :: [Int] -> [UMLStateDiagram] -> Bool
isConnFromNotHistory [] _ = True
isConnFromNotHistory [x] a = all (isNotHistory x) a
isConnFromNotHistory (x:xs) a = isConnFromNotHistory xs (getSubstate x a)

isNotHistory :: Int -> UMLStateDiagram -> Bool
isNotHistory a History {label}  = a /= label
isNotHistory _ _ = True
             
               --checkEmptyConnPoint 
checkEmptyConnPoint :: UMLStateDiagram -> Bool
checkEmptyConnPoint = all (\cs -> not (any (null . pointFrom) cs) && not (any (null . pointTo) cs))
                                                                                 
--checkJoint
checkJoint :: UMLStateDiagram -> Maybe String
checkJoint a
  | not(checkInEdge a) = Just ("Error: No Joint node should be reached by two connections "
    ++ "with different transition label.")
  |not(checkOutEdge a) = Just ("Error: No Joint node should be left by two connections "
    ++ "with different transition labels.")
  | not (all goIntoParallelRegions joints)
  = Just "at least one Joint has connections to states of the same region"
  | not (all comeOutOfParallelRegions joints)
  = Just "states from the same region connect to the same Joint"
  | otherwise = Nothing
  where
    joints = addressesOfJoints a
    goIntoParallelRegions    x = checkParallelRegionConnections True x a
    comeOutOfParallelRegions x = checkParallelRegionConnections False x a

checkInEdge :: UMLStateDiagram -> Bool
checkInEdge s@StateDiagram {} =
                        all (`checkInTran` onlyJoint) onlyJoint
                        where
                             global = globalise s
                             sub = substate global
                             conn = connection global
                             onlyJoint = filter (not.(`notJoint` sub).pointTo) conn
checkInEdge _ = True

checkInTran :: Connection -> [Connection] -> Bool
checkInTran a b = null tranNotSame 
                where
                  toSame    = filter ((pointTo a ==).pointTo) b
                  tranNotSame = filter ((transition a /=).transition) toSame

checkOutEdge :: UMLStateDiagram -> Bool
checkOutEdge s@StateDiagram {} =
                        all (`checkOutTran` onlyJoint) onlyJoint
                        where
                             global = globalise s
                             sub = substate global
                             conn = connection global
                             onlyJoint = filter (not.(`notJoint` sub).pointFrom) conn
checkOutEdge _ = True

checkOutTran :: Connection -> [Connection] -> Bool
checkOutTran a b = null tranNotSame 
                where
                  fromSame    = filter ((pointFrom a ==).pointFrom) b
                  tranNotSame = filter ((transition a /=).transition) fromSame

checkParallelRegionConnections :: Bool -> [Int] -> UMLStateDiagram -> Bool
checkParallelRegionConnections into l s = 0 == connections g {
  connection = [ Connection a b "" | a <- insides, b <- insides, a < b ]
  }
  where
    g = globalise s
    connections = foldr ((+) . length) 0 . localise
    insideCandidates = map inside . filter ((== l) . outside) $ connection g
    insides
      | xs@(_:_:_) <- insideCandidates = xs
      | otherwise                      = []
    (outside, inside)
      | into      = (pointFrom, pointTo)
      | otherwise = (pointTo, pointFrom)

addressesOfJoints :: UMLStateDiagram -> [[Int]]
addressesOfJoints s = case s of
  StateDiagram {} -> map tail $ addressesOfJoints' s
  _               -> []
  where
    addressesOfJoints' Joint {label} = [[label]]
    addressesOfJoints' StateDiagram {label, substate} =
      map (label:) $ concatMap addressesOfJoints' substate
    addressesOfJoints' CombineDiagram {label, substate} =
      map (label:) $ concatMap addressesOfJoints' substate
    addressesOfJoints' _ = []

--checkSemantics
checkSemantics :: UMLStateDiagram -> Maybe String
checkSemantics a
  | not(checkSameConnection a) = Just ("Error: No two connections are allowed leaving"
    ++ "the same source and and having the same label (except From Joint Node).")
  | not(checkEmptyTran a) = Just ("Error: The non-Joint state which has more than one outgoing "
    ++ "connection, must have a non-empty transition label.")
  | otherwise = Nothing
    --checkSameConnection
checkSameConnection :: UMLStateDiagram -> Bool
checkSameConnection s@StateDiagram {} =
                        all (`checkSameOutTran` withoutJoint) withoutJoint
                        where
                          global = globalise s
                          sub = substate global
                          conn = connection global
                          withoutJoint = filter ((`notJoint ` sub).pointFrom) conn
checkSameConnection _ = True

checkSameOutTran :: Connection -> [Connection] -> Bool
checkSameOutTran a b = length tranSame == 1
                where
                  fromSame  = filter ((pointFrom a ==).pointFrom) b
                  tranSame = filter ((transition a ==).transition) fromSame

notJoint  :: [Int] -> [UMLStateDiagram] -> Bool
notJoint  [] _ = True
notJoint  [x] a = all (isNotJoint x) a
notJoint (x:xs) a = notJoint  xs (getSubstate x a)

isNotJoint :: Int -> UMLStateDiagram -> Bool
isNotJoint a Joint {label}  = a /= label
isNotJoint _ _ = True
    --checkEmptyTran
checkEmptyTran :: UMLStateDiagram -> Bool
checkEmptyTran s@StateDiagram {} =                         
                         (not . any (null . transition)) filterOneOut
                        where
                             global = globalise s
                             sub = substate global
                             conn = connection global
                             withoutJoint = filter ((`notJoint ` sub).pointFrom) conn
                             filterOneOut = filter (not.(`onlyOneOut` withoutJoint)) withoutJoint
checkEmptyTran _ = True

onlyOneOut :: Connection -> [Connection] -> Bool
onlyOneOut a b = length fromSame == 1 
            where
             fromSame  = filter ((pointFrom a ==).pointFrom) b 






      