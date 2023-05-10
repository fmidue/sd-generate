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
flatten d = StateDiagram (bridge (toImsCp' (globalise d))) 0 "" [] [0]
            -- currently flattens states one layer too high
            -- transitions are not taken care of wip yet because id remap is required
            -- which was the reason to introduce the ImsCp data type

withOnlyIms :: UMLStateDiagram -> Bool
withOnlyIms (StateDiagram {substate})
    =  and (True:map withOnlyIms' substate)
withOnlyIms _ = False

withOnlyIms' :: UMLStateDiagram -> Bool
withOnlyIms' (InnerMostState {}) = True
withOnlyIms' _ = False

bridge :: [ImsCp] -> [UMLStateDiagram]
bridge imss = let
              in
              [bridge' x | x <- imss]

bridge' :: ImsCp -> UMLStateDiagram
bridge' imscp = InnerMostState 0 (getLabelStr imscp) ""

toImsCp' :: UMLStateDiagram -> [ImsCp]
toImsCp' sd@(StateDiagram {label,name,substate})
   | withOnlyIms sd =
      let
      rt = InnerMostState label name ""
      in
      [imsCpCtr [rt,x]|x <- substate]
   | otherwise = crsImsCps (map toImsCp' substate)
toImsCp' (CombineDiagram {label, substate})
   =
   let imss = crsImsCps (map toImsCp' substate)
       rt = InnerMostState label "" ""
   in
   [ImsCp rt (Just x) | x <- imss]
toImsCp' ims@(InnerMostState {}) = [ImsCp ims Nothing]
toImsCp' _ = []

data ImsCp = ImsCp UMLStateDiagram (Maybe ImsCp)
     deriving (Eq)

instance Show ImsCp where
   show (ImsCp (InnerMostState {name, label}) (Just inmsCP))
    = name ++ " " ++ show label ++ " " ++ show inmsCP
   show (ImsCp (InnerMostState {name, label}) Nothing)
    = name ++ " " ++ show label ++ " " ++ "\n"
   show _ = "bad inmsCP instance"

imsCpCtr :: [UMLStateDiagram] -> ImsCp
imsCpCtr [] = error "must provide states"
imsCpCtr [x] = ImsCp x Nothing
imsCpCtr (x:xs) = ImsCp x (Just (imsCpCtr xs))

getImss :: ImsCp -> [UMLStateDiagram]
getImss (ImsCp x Nothing) = [x]
getImss (ImsCp x (Just y)) = x:getImss y

crsImsCps :: [[ImsCp]] -> [ImsCp]
crsImsCps [x] = x
crsImsCps (x:y:xs) = crsImsCps (crsImsCps'' x y:xs)
crsImsCps _ = error "shouldnt happen"

crsImsCps' :: [UMLStateDiagram] -> ImsCp -> ImsCp
crsImsCps' [] y = y
crsImsCps' [x] y = ImsCp x (Just y)
crsImsCps' (x:xs) y = ImsCp x (Just (crsImsCps' xs y))

crsImsCps'' :: [ImsCp] -> [ImsCp] -> [ImsCp]
crsImsCps'' [] ys = ys
crsImsCps'' xs [] = xs
crsImsCps'' xs ys = [crsImsCps' (getImss x) y | x <- xs, y <- ys]



class Labeled a where
   getLabelStr :: a -> String

instance Labeled UMLStateDiagram where
    getLabelStr (StateDiagram {name}) = name
    getLabelStr (CombineDiagram {}) = error "not defined"
    getLabelStr (EndState {}) = error "not defined"
    getLabelStr (Joint {}) = error "not defined"
    getLabelStr (History {}) = error "not defined"
    getLabelStr (InnerMostState {name}) = name

instance Labeled ImsCp where
    getLabelStr (ImsCp x Nothing) = getLabelStr x
    getLabelStr (ImsCp x (Just s)) = getLabelStr x ++ ", " ++ getLabelStr s

instance Labeled Connection where
   getLabelStr (Connection {pointFrom, pointTo, transition}) = transition ++ show pointFrom ++ show pointTo

