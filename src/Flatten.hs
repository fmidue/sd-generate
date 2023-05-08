{-# LANGUAGE NamedFieldPuns            #-}

module Flatten (
   transitionLiterals
) where
import Datatype (UMLStateDiagram
                ,StateDiagram(..)
                ,Connection (..))
import Data.List (nub
                 ,sort)
-- import Example (verySmall)

{- performs a bfs reachability transform 
   of a hierarchical state chart and flatten it
-}

{- it is difficult to walk a structure that is not in some
   sort of normal form to rely on                       -}

{- wahrscheinlich etwas leichter mit eigenem Datentypen
   anstatt Int, direkte Referenzen auf verbundene Objekte
   reduziert total amount of queries required
flatten :: UMLStateDiagram -> UMLStateDiagram
flatten (StateDiagram {}) = verySmall
flatten (CombineDiagram {}) = verySmall
flatten (EndState {}) = verySmall
flatten (Joint {}) = verySmall
flatten (History {}) = verySmall
flatten (InnerMostState {}) = verySmall
-}

{- use for simulation of diagram -}
transitionLiterals :: UMLStateDiagram -> [String]
transitionLiterals diag = sort [x |x <- nub $ getTransitionLiterals diag [""], x /= ""]

getTransitionLiterals :: UMLStateDiagram -> [String] -> [String]
getTransitionLiterals (StateDiagram [] _ _ [] _) _ = [""]
getTransitionLiterals sd@(StateDiagram (x:xs) _ _ [] _) lits =
   getTransitionLiterals x lits ++ getTransitionLiterals (sd { substate = xs }) []
getTransitionLiterals sd@(StateDiagram _ _ _ (c:cs) _) lits
   = case c of
         Connection {transition} -> getTransitionLiterals (sd { connection = cs }) [] ++ (transition:lits)
getTransitionLiterals (CombineDiagram [] _) _ = [""]
getTransitionLiterals cd@(CombineDiagram (x:xs) _) _
   = getTransitionLiterals x [""] ++ getTransitionLiterals (cd {substate = xs}) [""]
getTransitionLiterals _ lits = lits

{- apply an inefficient but easy approach of using all available 
   transitions -}

-- data set of nodes
-- data set of connection

