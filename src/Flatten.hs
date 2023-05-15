{-# LANGUAGE NamedFieldPuns            #-}

module Flatten (
   flatten
   ,mergeStates
   ,truncateUpperLabels
   ,redistributeLabels
) where
import Datatype (UMLStateDiagram
                ,StateDiagram(..)
                ,globalise
                --,Connection(..)
                )


flatten :: UMLStateDiagram -> [([[Int]], UMLStateDiagram)]
flatten d = let
            g = globalise d
            in
            redistributeLabels (mergeStates (truncateUpperLabels (flatten' g)))

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
flatten' parent@(StateDiagram {substate = substates})
    | withOnlyInnerMost parent = inheritFrom substates parent
    | otherwise = inheritFrom' (concatMap flatten' substates) parent
flatten' parent@(CombineDiagram{substate = substates})
    = inheritFrom' (cartesianProduct $ map flatten' substates) parent
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

{-
  next steps; redistribute fresh labels onto flattened states
  now use [[Int]] ~ label as a lookup table to remap the transitions correctly
-}

{-
  identify 1:1, 1:N (fork), N:1 (join) and restore
-}

{-
extractOneToOne :: UMLStateDiagram -> [Connection]
extractOneToOne (StateDiagram {connection})
    = [ c| c@(Connection{ pointFrom, pointTo}) <- connection
    , length pointFrom == 1
    , length pointTo == 1 ]
extractOneToOne _ = []


restoreOneToOne :: [([[Int]], UMLStateDiagram)] -> [Connection] -> [Connection]
restoreOneToOne flat cs = [  |fl <- flat, c@(Connection{pointFrom,pointTo}) <- cs, pointFrom  ]
-}
