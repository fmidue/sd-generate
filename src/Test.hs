module Test where
import Datatype
import Layout
import Data.List
import Data.Maybe

--check Connection Points
checkConnection :: UMLStateDiagram -> Maybe String 
checkConnection a 
  | not(checkSubC a) = Just ("Error:  Connection Points")
  | otherwise = Nothing

checkSubC :: UMLStateDiagram -> Bool
checkSubC  s@StateDiagram {} =  isContained (getConnectionPointFrom (connection s)) (getLayerList (substate s) ) (substate s) && isContained (getConnectionPointTo (connection s)) (getLayerList (substate s)) (substate s) && all checkSubC (substate s)
checkSubC  _ = True

getConnectionPointFrom :: [Connection] -> [[Int]]
getConnectionPointFrom [] = []
getConnectionPointFrom (x:xs) = (getPointFrom x) ++ (getConnectionPointFrom xs)

getPointFrom :: Connection -> [[Int]]
getPointFrom c@Connection {} = inits (pointFrom c)

getConnectionPointTo :: [Connection] -> [[Int]]
getConnectionPointTo [] = []
getConnectionPointTo (x:xs) = (getPointTo x) ++ (getConnectionPointTo xs)

getPointTo :: Connection -> [[Int]]
getPointTo c@Connection {} = inits (pointTo c)

isContained :: [[Int]] -> [Int] -> [UMLStateDiagram] -> Bool
isContained [] _ _= True
isContained (x:xs) a b = isContained1 x a b && isContained xs a b

isContained1 :: [Int] -> [Int] -> [UMLStateDiagram] -> Bool
isContained1 [] _ _ = True
isContained1 (x:xs) a b =  (x `elem` a)  &&  isContained1 xs (getLayerList (fromJust(getSubstate x b)))  (fromJust(getSubstate x b))

getSubstate :: Int -> [UMLStateDiagram] -> Maybe [UMLStateDiagram]
getSubstate _ [] = Just []
getSubstate a (x:xs) = case (a `elem` (getLabel x))  of 
                                                        True -> Just (getSubstate1 x)
                                                        False -> (getSubstate a xs)

getSubstate1 :: UMLStateDiagram -> [UMLStateDiagram]
getSubstate1 (StateDiagram a _ _ _ _) = a
getSubstate1 (CombineDiagram a _) = a
getSubstate1 (Joint _) = []
getSubstate1 (History _ _) = []
getSubstate1 (InnerMostState _ _ _ ) = []


-- check local uniqueness 
checkUniqueness :: UMLStateDiagram -> Maybe String 
checkUniqueness a 
  | not(checkSub a) = Just ("Error: Local Uniqueness ")
  | otherwise = Nothing

checkSub :: UMLStateDiagram -> Bool
checkSub  s@StateDiagram {} =  isUnique (getLayerList (substate s)) && all checkSub (substate s)
checkSub  s@CombineDiagram {} =  isUnique (getLayerList (substate s)) && all checkSub (substate s)
checkSub  _ = True

getLayerList :: [UMLStateDiagram] -> [Int]
getLayerList [] = []
getLayerList (x:xs) = getLable x ++ getLayerList xs

getLabel :: UMLStateDiagram -> [Int]
getLabel (StateDiagram _ a _ _ _) = [a]
getLabel (CombineDiagram _ a) = [a]
getLabel (Joint a) = [a]
getLabel (History a _) = [a]
getLabel (InnerMostState a _ _ ) = [a]

isUnique :: [Int] -> Bool
isUnique a =((length a) ==  length (nub a))


checkValidity :: UMLStateDiagram -> Maybe String
checkValidity a
  | not(checkOuterMostLayer a) = Just ("Error: Outermost layer must be 'StateD"
    ++ "iagram' or 'CombineDiagram' constructor")
  | not(checkSubstateSD a) = Just ("Error: Substate of StateDiagram constructo"
    ++ "r cannot be empty or just History/Joint")
  | not(checkSubstateCD a) = Just ("Error: CombineDiagram constructor must con"
    ++ "tain at least 2 StateDiagram and no other type of constructor")
  | otherwise = Nothing

checkOuterMostLayer :: UMLStateDiagram -> Bool
checkOuterMostLayer Joint {} = False
checkOuterMostLayer History {} = False
checkOuterMostLayer InnerMostState {} = False
checkOuterMostLayer _ = True

checkSubstateSD :: UMLStateDiagram -> Bool
checkSubstateSD (StateDiagram [] _ _ _ _) = False
checkSubstateSD (CombineDiagram a _) = all checkSubstateSD a
checkSubstateSD (StateDiagram a _ _ _ _) = any checkListInSD a && all
  checkSubstateSD a
checkSubstateSD _ = True

checkListInSD :: UMLStateDiagram -> Bool
checkListInSD Joint {} = False
checkListInSD History {} = False
checkListInSD InnerMostState {} = True
checkListInSD CombineDiagram {} = True
checkListInSD StateDiagram {} = True

checkSubstateCD :: UMLStateDiagram -> Bool
checkSubstateCD (CombineDiagram [] _) = False
checkSubstateCD (CombineDiagram [_] _) = False
checkSubstateCD (CombineDiagram a _) = all checkListInCD a
checkSubstateCD (StateDiagram a _ _ _ _) = all checkSubstateCD a
checkSubstateCD _ = True

checkListInCD :: UMLStateDiagram -> Bool
checkListInCD Joint {} = False
checkListInCD History {} = False
checkListInCD InnerMostState {} = False
checkListInCD CombineDiagram {} = False
checkListInCD (StateDiagram a _ _ _ _) = all checkSubstateCD a

checkWrapper :: UMLStateDiagram -> Maybe String
checkWrapper a
  | not(checkOuterMostWrapper b) = Just ("Error: Outermost layer must be 'OrDe"
    ++ "com' constructor")
  | not(checkOrDecomSubstate b) = Just ("Error: Substate of OrDecom constructo"
    ++ "r cannot be empty or just Hist/Fork/StartS/Dummy/Transition")
  | not(checkSubstateCD a) = Just ("Error: AndDecom constructor must con"
    ++ "tain at least 2 OrDecom and no other type of constructor")
  | not(checkLayout b) = Just ("Error: Horizontal slicing must be followed by "
    ++ "vertical layering or vise versa")
  | otherwise = Nothing
    where
      b = addDummy $ getWrapper $ rearrangeSubstate a

checkOuterMostWrapper :: Wrapper -> Bool
checkOuterMostWrapper OrDecom {} = True
checkOuterMostWrapper AndDecom {} = True
checkOuterMostWrapper _ = False

checkOrDecomSubstate :: Wrapper -> Bool
checkOrDecomSubstate (OrDecom [] _ _ _ _ _ _ _ _) = False
checkOrDecomSubstate (AndDecom a _ _ _ _ _) = all checkOrDecomSubstate a
checkOrDecomSubstate (OrDecom a _ _ _ _ _ _ _ _) = any checkOrDecomList (concat a) &&
  all checkOrDecomSubstate (concat a)
checkOrDecomSubstate _ = True

checkOrDecomList :: Wrapper -> Bool
checkOrDecomList AndDecom {} = True
checkOrDecomList OrDecom {} = True
checkOrDecomList Leaf {} = True
checkOrDecomList _ = False

checkAndDecomSubstate :: Wrapper -> Bool
checkAndDecomSubstate (AndDecom [] _ _ _ _ _) = False
checkAndDecomSubstate (AndDecom [_] _ _ _ _ _) = False
checkAndDecomSubstate (AndDecom a _ _ _ _ _) = all checkAndDecomList a
checkAndDecomSubstate (OrDecom a _ _ _ _ _ _ _ _) = all checkAndDecomSubstate (concat a)
checkAndDecomSubstate _ = True

checkAndDecomList :: Wrapper -> Bool
checkAndDecomList (OrDecom a _ _ _ _ _ _ _ _) = all checkAndDecomSubstate (concat a)
checkAndDecomList _ = False

checkLayout :: Wrapper -> Bool
checkLayout a@(OrDecom [[b@AndDecom {}]] _ _ _ _ _ _ _ _) = layout a == layout b && checkLayout b
checkLayout (OrDecom a _ _ _ _ _ _ _ _) = all checkLayout (concat a)
checkLayout a@(AndDecom b _ _ _ _ _) = all (== layout a) (fmap layout b) && all checkLayout b
checkLayout _ = True
