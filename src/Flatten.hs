{-# LANGUAGE NamedFieldPuns            #-}

module Flatten (
   flatten
   ,mergeStates
   ,truncateUpperLabels
) where
import Datatype (UMLStateDiagram
                ,StateDiagram(..)

                ,globalise)


flatten :: UMLStateDiagram -> [Flattened]
flatten d = flatten' (globalise d)

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

mergeStates :: [(a, [UMLStateDiagram])] -> [(a, UMLStateDiagram)]
mergeStates flattened = [(xs, mergeStates' ys)|(xs,ys)<-flattened]

mergeStates' :: [UMLStateDiagram] -> UMLStateDiagram
mergeStates' innerStates = InnerMostState 0 (concat [name ++ ", "| InnerMostState{name}<-innerStates,name /= ""]) ""

truncateUpperLabels :: [([[a]], b)] -> [([[a]], b)]
truncateUpperLabels flattened = [([tail x|x<-xs],ys)|(xs,ys)<-flattened]

{-
  next steps; redistribute fresh labels onto flattened states
  now use [[Int]] ~ label as a lookup table to remap the transitions correctly
-}

flatten' :: UMLStateDiagram -> [Flattened]
flatten' parent@(StateDiagram {substate = substates})
    | withOnlyInnerMost parent = inheritFrom substates parent
    | otherwise = inheritFrom' (concatMap flatten substates) parent
flatten' parent@(CombineDiagram{substate = substates})
    = inheritFrom' (cartesianProduct $ map flatten substates) parent
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
