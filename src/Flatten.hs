{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE InstanceSigs              #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Flatten (
   flatten
   ,mergeStates
   ,truncateUpperLabels
   ,redistributeLabels
   ,groupSameConnections
   ,getConnections
   ,Flattable
   ,FlatUMLStateDiagram
   ,FlatConnection
   ,inFlatForm
) where
import Datatype (UMLStateDiagram
                ,StateDiagram'(..)
                ,StateDiagram
                ,globalise
                ,Connection'(..)
                ,Connection
                )
import Data.List

type FlatUMLStateDiagram = StateDiagram' [[Int]] [FlatConnection]

type FlatConnection = Connection' [[Int]] [[Int]]

{- preperation for type change that will require conversion -}
class Flattable a b where
  inFlatForm :: a -> b
  -- inCompositeForm :: b -> a

instance Flattable UMLStateDiagram FlatUMLStateDiagram where
  inFlatForm :: UMLStateDiagram -> FlatUMLStateDiagram
  inFlatForm diagram
    = case diagram of
        (StateDiagram {substate, label, name, connection, startState})
          -> StateDiagram { substate = map inFlatForm substate
                         , label = [[label]]
                         , name = name
                         , connection = map inFlatForm connection
                         , startState = startState }
        (InnerMostState {label, name, operations})
          -> InnerMostState { label = [[label]]
                            , name = name
                            , operations = operations }
        (CombineDiagram {substate, label})
          -> CombineDiagram { substate = map inFlatForm substate
                            , label = [[label]] }
        (EndState {label})
          -> EndState { label = [[label]] }
        (Joint {label})
          -> Joint { label = [[label]] }
        (History {label, historyType})
          -> History { label = [[label]]
                     , historyType = historyType }

instance Flattable Connection FlatConnection where
  inFlatForm :: Connection -> FlatConnection
  inFlatForm (Connection {pointFrom, pointTo, transition })
    = Connection { pointFrom = [pointFrom]
                 , pointTo = [pointTo]
                 , transition = transition}

flatten :: UMLStateDiagram -> UMLStateDiagram
flatten d = let
            g = globalise d
            newDiag = redistributeLabels $
                      mergeStates $
                      truncateUpperLabels $
                      flatten' g
            newCons = restoreConnections' (redistributeLabels (mergeStates (truncateUpperLabels (flatten' g)))) (getConnections g)
            newStates = map snd newDiag
            in
            StateDiagram { substate = newStates, connection = newCons, label = 0, startState = [1], name = ""}

withOnlyInnerMost :: UMLStateDiagram -> Bool
withOnlyInnerMost (StateDiagram {substate})
    =  and (True:map withOnlyInnerMost' substate)
withOnlyInnerMost _ = False

withOnlyInnerMost' :: UMLStateDiagram -> Bool
withOnlyInnerMost' (InnerMostState {}) = True
withOnlyInnerMost' _ = False

type Flattened = ([[Int]],[UMLStateDiagram])

castToInner :: StateDiagram a1 -> StateDiagram a2
castToInner (StateDiagram {label, name}) = InnerMostState label name ""
castToInner (CombineDiagram {label}) = InnerMostState label "" ""
castToInner _ = InnerMostState 999 "unknown" "cast"

getLabel :: StateDiagram a -> Int
getLabel (StateDiagram {label}) = label
getLabel (CombineDiagram {label}) = label
getLabel _ = 0

mergeStates :: [([[Int]], [UMLStateDiagram])] -> [([[Int]], UMLStateDiagram)]
mergeStates flattened = [(xs, mergeStates' ys)|(xs,ys)<-flattened]

mergeStates' :: [UMLStateDiagram] -> UMLStateDiagram -- op order is important! type change
mergeStates' innerStates = InnerMostState 0 (concat [name ++ ", "| InnerMostState{name}<-innerStates,name /= ""]) ""

truncateUpperLabels :: [Flattened] -> [Flattened]
truncateUpperLabels flattened = [([tail x|x<-xs],ys)|(xs,ys)<-flattened]

redistributeLabels :: [([[Int]], UMLStateDiagram)] -> [([[Int]], UMLStateDiagram)]
redistributeLabels xs = redistributeLabels' xs 1

redistributeLabels' :: [([[Int]], UMLStateDiagram)] -> Int -> [([[Int]], UMLStateDiagram)]
redistributeLabels' ((x,y):xs) i = (x,(y {label = i})) : redistributeLabels' xs (i + 1)
redistributeLabels' _ _ = []

flatten' :: UMLStateDiagram -> [Flattened]
flatten' parent@(StateDiagram {substate})
    | withOnlyInnerMost parent = inheritFrom substate parent
    | otherwise = inheritFrom' (concatMap flatten' substate) parent
flatten' parent@(CombineDiagram{substate})
    = inheritFrom' (cartesianProduct $ map flatten' substate) parent
flatten' innerMost@(InnerMostState {label}) = [([[label]],[innerMost])]
flatten' _ = []

inheritFrom :: [UMLStateDiagram] -> UMLStateDiagram -> [Flattened]
inheritFrom substates outer
    = [([[getLabel outer, label]], [castToInner outer, inner])|inner@InnerMostState{label}<-substates]

inheritFrom' :: [Flattened] -> UMLStateDiagram -> [Flattened]
inheritFrom' substates outer
    = [(map (getLabel outer:) labels, castToInner outer:innerStates)|(labels,innerStates)<-substates]

cartesianProduct' :: [Flattened] -> [Flattened] -> [Flattened]
cartesianProduct' [] _ = []
cartesianProduct' _ [] = []
cartesianProduct' ((label,states):xs) ys
    = [ (label ++ label',states ++ states') | (label',states') <-ys ] ++ cartesianProduct' xs ys

cartesianProduct :: [[Flattened]] -> [Flattened]
cartesianProduct [] = []
cartesianProduct [x] = x
cartesianProduct (x:y:xs) = cartesianProduct (cartesianProduct' x y : xs)

getConnections :: UMLStateDiagram -> [Connection]
getConnections (StateDiagram {connection}) = connection
getConnections _ = []

restoreConnections' :: [([[Int]], UMLStateDiagram)] -> [Connection] -> [Connection]
restoreConnections' flat connections
    = [ Connection
        { pointFrom = [newSource]
        , pointTo = [newTarget]
        , transition = extractTransitionName transitionGroup }
       | (srcLabels, InnerMostState {label = newSource}) <- flat
         , transitionGroup <- groupSameConnections connections
         , (_,InnerMostState {label = newTarget}) <- [tgt | tgt@(tgtLabels,_) <- flat
         , sort tgtLabels == sort (replaceMatching srcLabels (asSourceToTarget transitionGroup))
         , replaceMatching srcLabels (asSourceToTarget transitionGroup) /= srcLabels ] ]

replaceMatching :: [[Int]] -> [([Int],[Int])] -> [[Int]]
replaceMatching [] _ = []
replaceMatching (x:xs) ys = if null possibleTarget then x : replaceMatching xs ys
                            else possibleTarget ++ replaceMatching xs ys
                            where
                            possibleTarget = [y'|(y,y')<-ys, x == y]

extractTransitionName :: [Connection] -> String
extractTransitionName [] = "should never happen"
extractTransitionName ((Connection {transition}):_) = transition

asSourceToTarget :: [Connection] -> [([Int],[Int])]
asSourceToTarget [] = []
asSourceToTarget ((Connection {pointTo,pointFrom}):xs) = (pointFrom,pointTo):asSourceToTarget xs

groupSameConnections :: [Connection] -> [[Connection]]
groupSameConnections = groupBy transitionName' . sortBy transitionName

transitionName :: Connection -> Connection -> Ordering
transitionName (Connection{transition}) (Connection{transition=transition'})
    | transition >= transition' = GT
    | otherwise = LT

transitionName' :: Connection -> Connection -> Bool
transitionName' (Connection{transition}) (Connection{transition=transition'})
    = transition == transition'

