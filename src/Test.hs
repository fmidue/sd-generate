{-# LANGUAGE NamedFieldPuns #-}
module Test
  ( checkStructure
  , checkConnection
  , checkCrossings
  , checkNameUniqueness
  , checkUniqueness
  , checkEndState
  , checkStartState
  , checkJoint
  , checkHistory
  , checkSemantics
  ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram,
  globalise,
  localise,
  )
import Helper
import Data.List.Extra

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
  | not(checkReachablity a) 
  = Just "Should Not contain unreachable states (except Start states)" 
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
checkHistConnTransition Connection { pointFrom,transition } a = notHistory pointFrom a || null transition

                 --checkReachablity
checkReachablity :: UMLStateDiagram -> Bool
checkReachablity s@StateDiagram{} 
  = all (`elem` (map pointTo conn ++ globalStart s)) stateNoCDSD
    where
      global = globalise s
      conn   = connection global 
      sub    = substate global
      stateNoCDSD = filter (not.(`isSDCD` sub)) (getAllElem s)
checkReachablity _ = True

               --checkEmptyConnPoint 
checkEmptyConnPoint :: UMLStateDiagram -> Bool
checkEmptyConnPoint = all (\cs -> not (any (null . pointFrom) cs) && not (any (null . pointTo) cs))

--checkConnection
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

checkConnFromToRegion :: UMLStateDiagram -> Bool
checkConnFromToRegion StateDiagram{substate,connection}  = 
                                all ((`lastSecNotCD` substate) . pointFrom) connection
                             && all ((`lastSecNotCD` substate) . pointTo) connection
                             && all checkConnFromToRegion substate
checkConnFromToRegion CombineDiagram {substate} = all checkConnFromToRegion substate
checkConnFromToRegion _ = True
                        
checkCrossings :: UMLStateDiagram -> Maybe String
checkCrossings s = case connections s - connections (localise s) of
  0 -> Nothing
  n -> Just $ "Has " ++ show n ++ " illegal crossing(s) between regions"
  where
    connections = foldr ((+) . length) 0

--checkNameUniqueness
checkNameUniqueness :: UMLStateDiagram -> Maybe String
checkNameUniqueness a
  | not(checkSubNameUniq a) = Just "Error: At the same layer name Uniqueness is not fullfilled "
  | not(checkSDNameUniq a) = Just ("Error: In each StateDiagram, the name (if not empty) should be different "
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

--checkUniqueness
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

--checkEndState
checkEndState :: UMLStateDiagram -> Maybe String
checkEndState a
  | not(checkEndOutEdges a) = Just "Error: no EndState should have outgoing edges"
  | otherwise = Nothing

checkEndOutEdges :: UMLStateDiagram -> Bool
checkEndOutEdges StateDiagram { substate, connection } = all ((`isNotEnd` substate) . pointFrom) connection
                                                         && all checkEndOutEdges substate
checkEndOutEdges CombineDiagram {substate} = all checkEndOutEdges substate
checkEndOutEdges  _ = True

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
                                               lastSecNotCD startState substate 
                                             && all checkStartToRegion substate
checkStartToRegion CombineDiagram {substate} = all checkStartToRegion substate
checkStartToRegion _ = True                                          
                                                                                
--checkJoint
checkJoint :: UMLStateDiagram -> Maybe String
checkJoint a
  | not(checkTransition a) = Just "transitions from/to some Joints are not in line with standards. "
  | not(checkMtoOne a) = Just "invalid number of Outgoing edges or ingoing edges of Joint node"
  | not (all goIntoParallelRegions joints)
  = Just "at least one Joint has connections to states of the same region"
  | not (all comeOutOfParallelRegions joints)
  = Just "states from the same region connect to the same Joint"
  | otherwise = Nothing
  where
    joints = addressesOfJoints a
    goIntoParallelRegions    x = checkParallelRegionConnections True x a
    comeOutOfParallelRegions x = checkParallelRegionConnections False x a

checkMtoOne :: UMLStateDiagram -> Bool
checkMtoOne s@StateDiagram{} = 
   null (toMany `intersect` fromMany)
   && all (`notElem` toOnlyJoint) startOnlyJoint
   && all (`elem` fromMany) startOnlyJoint
   && all (`elem` startOnlyJoint) toNoConn
   && null fromNoConn 
   && null (toOne `intersect` fromOne)
      where 
        global = globalise s
        sub    = substate global
        conn   = connection global
        start  = globalStart global
        toOnlyJoint   = map pointTo (filter (not.(`notJoint` sub).pointTo) conn)
        fromOnlyJoint = map pointFrom (filter (not.(`notJoint` sub).pointFrom) conn)
        startOnlyJoint = filter (not.(`notJoint` sub)) start
        toMany        = filter (`overOne` toOnlyJoint) toOnlyJoint
        fromMany      = filter (`overOne`  fromOnlyJoint) fromOnlyJoint
        toOne         = toOnlyJoint \\ toMany
        fromOne       = fromOnlyJoint \\fromMany
        toNoConn = nub fromOnlyJoint \\ nub toOnlyJoint
        fromNoConn = nub toOnlyJoint \\ nub fromOnlyJoint
checkMtoOne _ = True

overOne :: [Int] -> [[Int]] -> Bool
overOne a b =  length (filter( a ==) b) > 1

checkTransition :: UMLStateDiagram -> Bool
checkTransition s@StateDiagram {} =
  all (`checkOutTran` fromOnlyJoint) fromOnlyJoint 
  && all (`checkInTran` toOnlyJoint) toOnlyJoint
  && null (fromTranNonEmpty `intersect` toTranNonEmpty)
  && all (`notElem` fromTranNonEmpty) startOnlyJoint
    where
      global = globalise s
      sub = substate global
      conn = connection global
      start  = globalStart global
      fromOnlyJoint = filter (not.(`notJoint` sub).pointFrom) conn
      toOnlyJoint = filter (not.(`notJoint` sub).pointTo) conn
      startOnlyJoint = filter (not.(`notJoint` sub)) start
      fromTranNonEmpty = map pointFrom (filter (not.(`checkOutTranEmpty` fromOnlyJoint)) fromOnlyJoint)
      toTranNonEmpty = map pointTo (filter (not.(`checkInTranEmpty` toOnlyJoint)) toOnlyJoint)
checkTransition _ = True

checkOutTran :: Connection -> [Connection] -> Bool
checkOutTran a b = null tranNotSame 
                where
                  fromSame    = filter ((pointFrom a ==).pointFrom) b
                  tranNotSame = filter ((transition a /=).transition) fromSame

checkInTran :: Connection -> [Connection] -> Bool
checkInTran a b = null tranNotSame 
                where
                  toSame    = filter ((pointTo a ==).pointTo) b
                  tranNotSame = filter ((transition a /=).transition) toSame

checkOutTranEmpty :: Connection -> [Connection] -> Bool
checkOutTranEmpty a b = any (null.transition) (filter ((pointFrom a ==).pointFrom) b)

checkInTranEmpty :: Connection -> [Connection] -> Bool
checkInTranEmpty a b = any (null.transition) (filter ((pointTo a ==).pointTo) b)

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
    addressesOfJoints' StateDiagram {label, substate}   =
      map (label:) $ concatMap addressesOfJoints' substate
    addressesOfJoints' CombineDiagram {label, substate} =
      map (label:) $ concatMap addressesOfJoints' substate
    addressesOfJoints' _ = []

checkHistory :: UMLStateDiagram -> Maybe String
checkHistory a
  | not(checkInEdge a) 
  = Just "History should never be reached from (somewhere, possibly nested) inside their own compound state"
  | not(checkOutEdge a) 
  = Just "History should only have connections towards something in their own compound state or deeper"
  | otherwise = Nothing

checkInEdge :: UMLStateDiagram -> Bool
checkInEdge s@StateDiagram {} = 
     all (\Connection{pointFrom, pointTo} -> 
                       init (take (length pointTo) pointFrom) /= init pointTo) toOnlyHistory
                               where 
                                  global = globalise s
                                  sub    = substate global
                                  conn   = connection global
                                  toOnlyHistory = filter (not.(`notHistory` sub).pointTo) conn
checkInEdge _ = True

checkOutEdge :: UMLStateDiagram -> Bool
checkOutEdge s@StateDiagram {} = 
     all (\Connection{pointFrom, pointTo} -> 
                      init (take (length pointFrom) pointTo) == init pointFrom) fromOnlyHistory
                               where 
                                  global = globalise s
                                  sub    = substate global
                                  conn   = connection global
                                  fromOnlyHistory = filter (not.(`notHistory` sub).pointFrom) conn
checkOutEdge _ = True
--checkSemantics
checkSemantics :: UMLStateDiagram -> Maybe String
checkSemantics a
  | not(checkSameConnection a) = Just ("Error: No two connections are allowed leaving"
    ++ " the same source and and having the same label (except From Joint Node).")
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






      