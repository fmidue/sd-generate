{-# OPTIONS_GHC -Wno-error=incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-error=deprecations #-}
{-# LANGUAGE NamedFieldPuns #-}

module Modelling.StateDiagram.Checkers.ForkOrJoin (checkForkAndJoin) where

import Modelling.StateDiagram.Datatype (
  Connection(..),
  StateDiagram(..),
  UMLStateDiagram(unUML'),
  unUML,
  globalise,
  localise,
  )

import Modelling.StateDiagram.Checkers.Helpers (globalStart, notForkOrJoin, isFork, isJoin, overOne)
import Data.List.Extra

checkForkAndJoin :: UMLStateDiagram n Int -> Maybe String
checkForkAndJoin a
  | not (checkTransition a) =
      Just "transitions from/to some ForkOrJoins are not in line with standards."
  | not (checkManyToOne a) =
      Just "invalid number of Outgoing edges or ingoing edges of ForkOrJoin node"
  | not (all goIntoParallelRegions forkOrJoins) =
      Just "at least one ForkOrJoin has connections to states that are not in distinct parallel regions"
  | not (all comeOutOfParallelRegions forkOrJoins) =
      Just "states not from distinct parallel regions connect to the same ForkOrJoin"
  | otherwise =
      Nothing
  where
    forkOrJoins = addressesOfForkOrJoins a
    goIntoParallelRegions    x = checkParallelRegionConnections True x a
    comeOutOfParallelRegions x = checkParallelRegionConnections False x a

checkManyToOne :: UMLStateDiagram n Int -> Bool
checkManyToOne s =
   all (`isFork` sub) fromMany
   && all (`isJoin` sub) toMany
   && all (`notElem` toOnlyForkOrJoin) startOnlyForkOrJoin
   && all (`elem` fromMany) startOnlyForkOrJoin
   && all (`elem` startOnlyForkOrJoin) toNoConn
   && null fromNoConn
   && null (toOne `intersect` fromOne)
      where
        global = unUML' $ globalise s
        sub    = substates global
        conn   = connections global
        start  = globalStart global
        toOnlyForkOrJoin =
          filter (not.(`notForkOrJoin` sub)) (map pointTo conn)
        fromOnlyForkOrJoin =
          filter (not.(`notForkOrJoin` sub)) (map pointFrom conn)
        startOnlyForkOrJoin =
          filter (not.(`notForkOrJoin` sub)) start
        toMany        = filter (`overOne` toOnlyForkOrJoin) toOnlyForkOrJoin
        fromMany      = filter (`overOne`  fromOnlyForkOrJoin) fromOnlyForkOrJoin
        toOne         = toOnlyForkOrJoin \\ toMany
        fromOne       = fromOnlyForkOrJoin \\fromMany
        toNoConn = nub fromOnlyForkOrJoin \\ nub toOnlyForkOrJoin
        fromNoConn = nub toOnlyForkOrJoin \\ nub fromOnlyForkOrJoin

checkTransition :: UMLStateDiagram n Int -> Bool
checkTransition s =
  all (`checkOutTran` fromOnlyForkOrJoin) fromOnlyForkOrJoin
  && all (`checkInTran` toOnlyForkOrJoin) toOnlyForkOrJoin
  && null (fromTranNonEmpty `intersect` toTranNonEmpty)
  && all (`notElem` fromTranNonEmpty) startOnlyForkOrJoin
    where
      global = unUML' $ globalise s
      sub = substates global
      conn = connections global
      start  = globalStart global
      fromOnlyForkOrJoin = filter (not.(`notForkOrJoin` sub).pointFrom) conn
      toOnlyForkOrJoin = filter (not.(`notForkOrJoin` sub).pointTo) conn
      startOnlyForkOrJoin = filter (not.(`notForkOrJoin` sub)) start
      fromTranNonEmpty = map pointFrom $ filter
        (not.(`checkOutTranEmpty` fromOnlyForkOrJoin))
        fromOnlyForkOrJoin
      toTranNonEmpty = map pointTo $ filter
        (not.(`checkInTranEmpty` toOnlyForkOrJoin))
        toOnlyForkOrJoin

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

addressesOfForkOrJoins :: UMLStateDiagram n Int -> [[Int]]
addressesOfForkOrJoins = unUML
  $ \_ substates _ _ -> concatMap addressesOfForkOrJoins' substates
  where
    addressesOfForkOrJoins' Fork {label} = [[label]]
    addressesOfForkOrJoins' Join {label} = [[label]]
    addressesOfForkOrJoins' StateDiagram {label, substates}   =
      map (label:) $ concatMap addressesOfForkOrJoins' substates
    addressesOfForkOrJoins' CombineDiagram {label, substates} =
      map (label:) $ concatMap addressesOfForkOrJoins' substates
    addressesOfForkOrJoins' _ = []
