{-# OPTIONS_GHC -Wno-error=incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-error=deprecations #-}
{-# LANGUAGE NamedFieldPuns #-}

module Checkers.Joint ( checkJoint ) where

import Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram(unUML'),
  unUML,
  globalise,
  localise,
  )

import Checkers.Helpers (globalStart, notJoint)
import Data.List.Extra

checkJoint :: UMLStateDiagram n Int -> Maybe String
checkJoint a
  | not (checkTransition a) =
      Just "transitions from/to some Joints are not in line with standards."
  | not (checkManyToOne a) =
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

checkManyToOne :: UMLStateDiagram n Int -> Bool
checkManyToOne s =
   null (toMany `intersect` fromMany)
   && all (`notElem` toOnlyJoint) startOnlyJoint
   && all (`elem` fromMany) startOnlyJoint
   && all (`elem` startOnlyJoint) toNoConn
   && null fromNoConn
   && null (toOne `intersect` fromOne)
      where
        global = unUML' $ globalise s
        sub    = substates global
        conn   = connections global
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

overOne :: [Int] -> [[Int]] -> Bool
overOne a b =  length (filter ( a ==) b) > 1

checkTransition :: UMLStateDiagram n Int -> Bool
checkTransition s =
  all (`checkOutTran` fromOnlyJoint) fromOnlyJoint
  && all (`checkInTran` toOnlyJoint) toOnlyJoint
  && null (fromTranNonEmpty `intersect` toTranNonEmpty)
  && all (`notElem` fromTranNonEmpty) startOnlyJoint
    where
      global = unUML' $ globalise s
      sub = substates global
      conn = connections global
      start  = globalStart global
      fromOnlyJoint = filter (not.(`notJoint` sub).pointFrom) conn
      toOnlyJoint = filter (not.(`notJoint` sub).pointTo) conn
      startOnlyJoint = filter (not.(`notJoint` sub)) start
      fromTranNonEmpty = map pointFrom (filter (not.(`checkOutTranEmpty` fromOnlyJoint)) fromOnlyJoint)
      toTranNonEmpty = map pointTo (filter (not.(`checkInTranEmpty` toOnlyJoint)) toOnlyJoint)

checkOutTran :: Eq a => Connection a -> [Connection a] -> Bool
checkOutTran a b = null tranNotSame
                where
                  fromSame    = filter ((pointFrom a ==).pointFrom) b
                  tranNotSame = filter ((transition a /=).transition) fromSame

checkInTran :: Eq a => Connection a -> [Connection a] -> Bool
checkInTran a b = null tranNotSame
                where
                  toSame    = filter ((pointTo a ==).pointTo) b
                  tranNotSame = filter ((transition a /=).transition) toSame

checkOutTranEmpty :: Eq a => Connection a -> [Connection a] -> Bool
checkOutTranEmpty a b = any (null.transition) (filter ((pointFrom a ==).pointFrom) b)

checkInTranEmpty :: Eq a => Connection a -> [Connection a] -> Bool
checkInTranEmpty a b = any (null.transition) (filter ((pointTo a ==).pointTo) b)

checkParallelRegionConnections :: Bool -> [Int] -> UMLStateDiagram n Int -> Bool
checkParallelRegionConnections into l s = all null . localise $
                                            case g of
                                              StateDiagram{} -> g { connections = [ Connection a b "" | a <- insides, b <- insides, a < b ] }
                                              _ -> error "not defined"
                                          where
                                            g = unUML' $ globalise s
                                            insideCandidates = map inside . filter ((== l) . outside) $ connections g
                                            insides
                                             | xs@(_:_:_) <- insideCandidates = xs
                                             | otherwise                      = []
                                            (outside, inside)
                                             | into      = (pointFrom, pointTo)
                                             | otherwise = (pointTo, pointFrom)

addressesOfJoints :: UMLStateDiagram n Int -> [[Int]]
addressesOfJoints = unUML (\_ substates _ _ -> concatMap addressesOfJoints' substates)
  where
    addressesOfJoints' Joint {label} = [[label]]
    addressesOfJoints' StateDiagram {label, substates}   =
      map (label:) $ concatMap addressesOfJoints' substates
    addressesOfJoints' CombineDiagram {label, substates} =
      map (label:) $ concatMap addressesOfJoints' substates
    addressesOfJoints' _ = []
