{-# LANGUAGE NamedFieldPuns            #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Flatten (
   flatten
   ,mergeStates
   ,truncateUpperLabels
   ,redistributeLabels
   ,restoreConnections
   ,groupSameConnections
   ,getConnections
) where
import Datatype (UMLStateDiagram
                ,StateDiagram'(..)
                ,StateDiagram
                ,globalise
                ,Connection(..)
                )
import Data.List

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

restoreConnections :: [([[Int]], UMLStateDiagram)] -> [Connection] -> [Connection]
restoreConnections flat cs
    = [Connection
         { pointFrom = [newSource]
         , pointTo = [newTarget]
         , transition = transition' } |
         (Connection { pointFrom
                     , pointTo
                     , transition = transition' }) <- cs
         , (srcLabels, InnerMostState {label = newSource}) <- flat
         , (tgtLabels, InnerMostState {label = newTarget}) <- flat
         , oldSource <- srcLabels
         , oldTarget <- tgtLabels
         , pointTo `notElem` srcLabels
         , pointFrom == oldSource
         , pointTo == oldTarget
         , pointFrom `notElem` tgtLabels]

restoreConnections' :: [([[Int]], UMLStateDiagram)] -> [Connection] -> [Connection]
restoreConnections' flat connections
    = let

      in
      [ Connection
         { pointFrom = [newSource]
         , pointTo = [newTarget]
         , transition = extractTransitionName groupedSame } |
         (srcLabels, InnerMostState {label = newSource}) <- flat
         , groupedSame <- groupSameConnections connections
         , (_,InnerMostState {label = newTarget}) <- [tgt | tgt@(tgtLabels,_) <- flat
         ,  areEqual tgtLabels (preciseTarget (retainIntersecting srcLabels groupedSame)) ] ]

areEqual :: [[Int]] -> [[Int]] -> Bool
areEqual xs ys = (xs `intersect` ys) == xs

extractTransitionName :: [Connection] -> String
extractTransitionName [] = "should never happen"
extractTransitionName ((Connection {transition}):_) = transition

retainIntersecting :: [[Int]] -> [Connection] -> [Connection]
retainIntersecting y x = [c|c<-x,y'<-y,hasOrigin y' c]

hasOrigin :: [Int] -> Connection -> Bool
hasOrigin y x = case x of
            (Connection{pointFrom}) -> pointFrom == y

preciseTarget :: [Connection] -> [[Int]]
preciseTarget [] = []
preciseTarget ((Connection {pointTo}):xs) = pointTo:preciseTarget xs

groupSameConnections :: [Connection] -> [[Connection]]
groupSameConnections = groupBy transitionName' . sortBy transitionName

transitionName :: Connection -> Connection -> Ordering
transitionName (Connection{transition}) (Connection{transition=transition'})
    | transition >= transition' = GT
    | otherwise = LT

transitionName' :: Connection -> Connection -> Bool
transitionName' (Connection{transition}) (Connection{transition=transition'})
    = transition == transition'

