{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE InstanceSigs              #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Flatten (
  FlatDiagram
 ,FlatCon
 ,flatten
) where
import Datatype (UMLStateDiagram
                ,StateDiagram'(..)
                ,StateDiagram
                ,globalise
                --,localise
                ,Connection'(..)
                ,Connection
                )
import Data.List(groupBy
                ,sortBy
                , sort)
import Data.Bifunctor(bimap
                     ,Bifunctor(second, first))

type FlatDiagram = StateDiagram' Int [FlatCon]

type FlatCon = Connection' [[Int]] [[Int]]

type Flattening = ([FlatDiagram],[Relabeled])

type Relabeled = ([[Int]],FlatDiagram)

flatten :: UMLStateDiagram -> UMLStateDiagram
flatten x = head (stomp [globalise x])

stomp :: [UMLStateDiagram] -> [UMLStateDiagram]
stomp diagram@[root@(StateDiagram{})]
  = if headNodeIsFlat diagram
    then diagram
    else stomp $ dequeueChildFromParent diagram
stomp stack@(InnerMostState{}:StateDiagram{}:_)
  = stomp $ enqueueChildToParent stack
stomp stack@(parent@StateDiagram{}:StateDiagram{}:_)
  = if headNodeIsFlat stack
    then let
         oldInner = parentSubstates stack
         newOldInner = relabel oldInner (freeLabelsIn (destination stack) (length oldInner))
         newInner = map snd newOldInner
         rewiredDirect = rewireDirect stack newOldInner
         -- rewiredInitial = rewireInitial rewiredDirect parent
         lifted = lift newInner rewiredDirect
         next = removeParent lifted
         in
         stomp next
    else
    stomp $ dequeueChildFromParent stack
stomp y = error $ "not part of scenario1" ++ show y

relabel :: [UMLStateDiagram] -> [Int] -> [(UMLStateDiagram, UMLStateDiagram)]
relabel = zipWith (curry
           (\case
              (inner@(InnerMostState{label=oldLabel}),newLabel)
                -> (inner,inner{label=newLabel})
           )
          )

headNodeIsFlat :: [UMLStateDiagram] -> Bool
headNodeIsFlat (x:xs)
  = case x of
      (StateDiagram {substate})
        -> foldr ((&&) .
           (\case
               (InnerMostState {}) -> True
               _ -> False))
           True substate
      _ -> error "only applicable to StateDiagram"

removeParent :: [UMLStateDiagram] -> [UMLStateDiagram]
removeParent (_:xs) = xs

destination :: [UMLStateDiagram] -> UMLStateDiagram
destination (StateDiagram{}:dest@(StateDiagram{}):xs)
  = dest

parentSubstates :: [UMLStateDiagram] -> [UMLStateDiagram]
parentSubstates (StateDiagram{substate}:xs)
  = substate

lift :: [UMLStateDiagram] -> [UMLStateDiagram] -> [UMLStateDiagram]
lift innerStates (parent@(StateDiagram{}):target@(StateDiagram{substate}):xs)
  = parent : (target { substate = substate ++ innerStates }) : xs

dequeueChildFromParent :: [UMLStateDiagram] -> [UMLStateDiagram]
dequeueChildFromParent (parent@(StateDiagram{substate=s:subs}):xs)
  = s : parent{substate=subs} : xs

enqueueChildToParent :: [UMLStateDiagram] -> [UMLStateDiagram]
enqueueChildToParent (inner@(InnerMostState{}):parent@(StateDiagram{substate}):xs)
  = parent{substate=substate ++ [inner]} : xs

freeLabelsIn :: UMLStateDiagram -> Int -> [Int]
freeLabelsIn = freeLabelsIn' 1

isUsedIn :: [UMLStateDiagram] -> Int -> Bool
isUsedIn sublist l
  = not $ foldr ((&&) . (\x -> label x /= l )) True sublist

freeLabelsIn' :: Int -> UMLStateDiagram -> Int -> [Int]
freeLabelsIn' x sd@(StateDiagram{substate}) n
  = if isUsedIn substate x
    then freeLabelsIn' (x+1) sd n
    else x : freeLabelsIn' (x+1) sd (n-1)
freeLabelsIn' _ _ 0 = []
freeLabelsIn' _ _ _ = error "only StateDiagram are allowed to be queried"

rewireDirect :: [UMLStateDiagram] -> [(UMLStateDiagram, UMLStateDiagram)] -> [UMLStateDiagram]
rewireDirect stack oldNewInner
  = init stack ++ [newRoot]
  where
  root = last stack
  newRoot = root -- TODO: continue here, rewire connections

labelPrefix :: [UMLStateDiagram] -> [Int]
labelPrefix
  = foldr (\case
             (StateDiagram {label}) -> (label:)
             (CombineDiagram {label}) -> (label:)
             _ -> error "unsupported container type")
    []
