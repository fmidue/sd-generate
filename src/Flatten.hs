{-# LANGUAGE NamedFieldPuns            #-}

module Flatten (
    getLabelStr
   ,flatten
) where
import Datatype (UMLStateDiagram
                ,StateDiagram(..)
                ,Connection (..)
                ,globalise)


flatten :: UMLStateDiagram -> UMLStateDiagram
flatten d = StateDiagram (bridge (toInnerMostCp' (globalise d))) 0 "" [] [0]

withOnlyInnerMost :: UMLStateDiagram -> Bool
withOnlyInnerMost (StateDiagram {substate})
    =  and (True:map withOnlyInnerMost' substate)
withOnlyInnerMost _ = False

withOnlyInnerMost' :: UMLStateDiagram -> Bool
withOnlyInnerMost' (InnerMostState {}) = True
withOnlyInnerMost' _ = False

bridge :: [InnerMostCp] -> [UMLStateDiagram]
bridge innerMost = let
              in
              [bridge' x | x <- innerMost]

bridge' :: InnerMostCp -> UMLStateDiagram
bridge' innerMostCp = InnerMostState 0 (getLabelStr innerMostCp) ""

toInnerMostCp' :: UMLStateDiagram -> [InnerMostCp]
toInnerMostCp' sd@(StateDiagram {label,name,substate})
   | withOnlyInnerMost sd =
      let
      rt = InnerMostState label name ""
      in
      [innerMostCpCtr [rt,x]|x <- substate]
   | otherwise = concat $ map toInnerMostCp' substate
toInnerMostCp' (CombineDiagram {label, substate})
   =
   let innerMost = crsInnerMostCps (map toInnerMostCp' substate)
       root = InnerMostState label "" ""
   in
   [InnerMostCp root (Just x) | x <- innerMost]
toInnerMostCp' innerMost@(InnerMostState {}) = [InnerMostCp innerMost Nothing]
toInnerMostCp' _ = []

data InnerMostCp = InnerMostCp UMLStateDiagram (Maybe InnerMostCp)
     deriving (Eq)

instance Show InnerMostCp where
   show (InnerMostCp (InnerMostState {name, label}) (Just inmsCP))
    = name ++ " " ++ show label ++ " " ++ show inmsCP
   show (InnerMostCp (InnerMostState {name, label}) Nothing)
    = name ++ " " ++ show label ++ " " ++ "\n"
   show _ = "bad instance"

innerMostCpCtr :: [UMLStateDiagram] -> InnerMostCp
innerMostCpCtr [] = error "must provide states"
innerMostCpCtr [x] = InnerMostCp x Nothing
innerMostCpCtr (x:xs) = InnerMostCp x (Just (innerMostCpCtr xs))

getInnerMosts :: InnerMostCp -> [UMLStateDiagram]
getInnerMosts (InnerMostCp x Nothing) = [x]
getInnerMosts (InnerMostCp x (Just y)) = x:getInnerMosts y

crsInnerMostCps :: [[InnerMostCp]] -> [InnerMostCp]
crsInnerMostCps [x] = x
crsInnerMostCps (x:y:xs) = crsInnerMostCps (crsInnerMostCps'' x y:xs)
crsInnerMostCps _ = error "shouldnt happen"

crsInnerMostCps' :: [UMLStateDiagram] -> InnerMostCp -> InnerMostCp
crsInnerMostCps' [] y = y
crsInnerMostCps' [x] y = InnerMostCp x (Just y)
crsInnerMostCps' (x:xs) y = InnerMostCp x (Just (crsInnerMostCps' xs y))

crsInnerMostCps'' :: [InnerMostCp] -> [InnerMostCp] -> [InnerMostCp]
crsInnerMostCps'' [] ys = ys
crsInnerMostCps'' xs [] = xs
crsInnerMostCps'' xs ys = [crsInnerMostCps' (getInnerMosts x) y | x <- xs, y <- ys]


{- TODO: replace w. getField polymorphic function from library -}
class Labeled a where
   getLabelStr :: a -> String

instance Labeled UMLStateDiagram where
    getLabelStr (StateDiagram {name}) = name
    getLabelStr (CombineDiagram {}) = error "not defined"
    getLabelStr (EndState {}) = error "not defined"
    getLabelStr (Joint {}) = error "not defined"
    getLabelStr (History {}) = error "not defined"
    getLabelStr (InnerMostState {name}) = name

instance Labeled InnerMostCp where
    getLabelStr (InnerMostCp x Nothing) = getLabelStr x
    getLabelStr (InnerMostCp x (Just s)) = getLabelStr x ++ ", " ++ getLabelStr s

instance Labeled Connection where
   getLabelStr (Connection {pointFrom, pointTo, transition}) = transition ++ show pointFrom ++ show pointTo

