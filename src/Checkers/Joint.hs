{-# LANGUAGE NamedFieldPuns #-}

module Checkers.Joint ( checkJoint ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram,
  globalise,
  localise,
  )

import Checkers.Helpers (globalStart, notJoint)
import Data.List.Extra

checkJoint :: UMLStateDiagram -> Maybe String
checkJoint a
  | not (checkTransition a) =
      Just "transitions from/to some Joints are not in line with standards."
  | not (checkMtoOne a) =
      Just "invalid number of Outgoing edges or ingoing edges of Joint node"
  | not (all goIntoParallelRegions joints) =
      Just "at least one Joint has connections to states that are not in distinct parallel regions"
  | not (all comeOutOfParallelRegions joints) =
      Just "states not from distinct parallel regions connect to the same Joint"
  | otherwise =
      Nothing
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
        toOnlyJoint   = filter (not.(`notJoint` sub)) (map pointTo conn)
        fromOnlyJoint = filter (not.(`notJoint` sub)) (map pointFrom conn)
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
checkParallelRegionConnections into l s = all null . localise $ g {
  connection = [ Connection a b "" | a <- insides, b <- insides, a < b ]
  }
  where
    g = globalise s
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
