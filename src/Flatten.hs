{-# LANGUAGE NamedFieldPuns            #-}

module Flatten (
  crossInms
  ,unionTrns
  ,mrgInmsCP
) where
import Datatype (UMLStateDiagram
                ,StateDiagram(..)
                ,Connection (..))

{-
flatten :: UMLStateDiagram -> UMLStateDiagram
flatten (StateDiagram { substate = (x:xs)}) = let
                                              -- apply outermost transitions
                                              in
                                              
flatten _ = error "invalid UMLStateDiagram"

flatten' :: UMLStateDiagram
flatten' sd@(StateDiagram { substate = (x:xs)}) | sd `withOnlyInms` = 
flatten' (CombineDiagram {}) = error "invalid UMLStateDiagram"
flatten' (EndState {}) = error "invalid UMLStateDiagram"
flatten' (Joint {}) = error "invalid UMLStateDiagram"
flatten' (History {}) = error "invalid UMLStateDiagram"
flatten' (InnerMostState {}) = error "invalid UMLStateDiagram"
-}

{- arbitrarily long cp's of states -}
data InmsCP = InmsCP UMLStateDiagram (Maybe InmsCP)
     deriving (Eq)

instance Show InmsCP where
   show (InmsCP (InnerMostState {name}) (Just inmsCP)) = name ++ " " ++ show inmsCP
   show (InmsCP (InnerMostState {name}) Nothing) = name ++ "\n"
   show _ = "bad inmsCP instance"

{- form the cross product of substate regions.
   should only be applied when substates only consist of InnerMostStates
   hence the name Inms := InnerMostStates
-}
crossInms :: [[UMLStateDiagram]] -> [InmsCP]
crossInms [] = []
crossInms [x] = [InmsCP y Nothing | y <- x ]
crossInms (x:xs) = [InmsCP y (Just z) | y <- x, z <- crossInms xs ]

{- unify the sets of transitions of two regions -}
unionTrns :: [UMLStateDiagram] -> [Connection]
unionTrns [] = []
unionTrns (x:xs) = case x of
                     (StateDiagram {connection}) -> connection ++ unionTrns xs
                     _ -> []

{- simple getter for labels on various data types
   to avoid having to pattern match when its cumbersome -}
class Labeled a where
   getLabelStr :: a -> String

instance Labeled UMLStateDiagram where
    getLabelStr (StateDiagram {name}) = name
    getLabelStr (CombineDiagram {}) = error "not defined"
    getLabelStr (EndState {}) = error "not defined"
    getLabelStr (Joint {}) = error "not defined"
    getLabelStr (History {}) = error "not defined"
    getLabelStr (InnerMostState {name}) = name

instance Labeled InmsCP where
    getLabelStr (InmsCP x Nothing) = getLabelStr x
    getLabelStr (InmsCP x (Just s)) = getLabelStr x ++ ", " ++ getLabelStr s

{- merge crossproducts of states that were obtained into
   a visually representable form, redistributes ids -}
mrgInmsCP :: [InmsCP] -> UMLStateDiagram
mrgInmsCP xs = StateDiagram (mrgInmsCP' xs 1) 0 "" [] []

mrgInmsCP' :: [InmsCP] -> Int -> [UMLStateDiagram]
mrgInmsCP' [] _ = []
mrgInmsCP' (x:xs) y = let i = InnerMostState y (getLabelStr x) ""
                      in
                      i:mrgInmsCP' xs (y+1)


{-
mapUTrnsToInmsCPs :: [Connection] -> [InmsCP] -> [InmsCP]
mapUTrnsToInmsCPs trs ims = 
-}

{-
   todo impl: create crossproducts of regions recursively from bottom up
              remap their transitions to their new states
              once at top level remap outermost transitions into cped states
   might require another type for storing CPS with ID that has not been folded
   into a visualizable state yet
-}