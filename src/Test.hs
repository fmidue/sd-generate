{-# LANGUAGE NamedFieldPuns #-}
module Test where
import Datatype
import Layout
import Data.List.Extra
--check semantics
checkSemantics :: UMLStateDiagram -> Maybe String
checkSemantics a
  | not(checkOutermostHistory a) = Just "Error: History does not make sense in the outermost stateDiagram "
  | otherwise = Nothing

checkOutermostHistory :: UMLStateDiagram -> Bool
checkOutermostHistory (StateDiagram a _ _ _ _) = all checkHistoryInSD a
checkOutermostHistory _ = True

checkHistoryInSD :: UMLStateDiagram -> Bool
checkHistoryInSD Joint {} = True
checkHistoryInSD History {} = False
checkHistoryInSD InnerMostState {} = True
checkHistoryInSD CombineDiagram {} = True
checkHistoryInSD StateDiagram {} = True

-- check if  start state is valid
checkStartState :: UMLStateDiagram -> Maybe String
checkStartState a
  | not(checkSubS a) = Just "Error: invalid start state "
  | otherwise = Nothing

checkSubS :: UMLStateDiagram -> Bool
checkSubS  StateDiagram { substate, startState} = checkStart && all checkSubS substate
                                              where
                                                getLayerList = map label substate
                                                checkStart = isContained1 startState getLayerList substate
checkSubS CombineDiagram {substate} = all checkSubS substate
checkSubS  _ = True


--check Connection Points
checkConnection :: UMLStateDiagram -> Maybe String
checkConnection a
  | not(checkSubC a) = Just "Error:  Connection Points"
  | otherwise = Nothing

checkSubC :: UMLStateDiagram -> Bool
checkSubC  StateDiagram { substate, connection } =  checkConnFrom && checkConnTo  && all checkSubC substate
                              where
                                getLayerList = map label substate
                                checkConnFrom = isContained (map pointFrom connection) getLayerList  substate
                                checkConnTo = isContained (map pointTo connection ) getLayerList substate
checkSubC CombineDiagram {substate} = all checkSubC substate
checkSubC  _ = True

isContained :: [[Int]] -> [Int] -> [UMLStateDiagram] -> Bool
isContained [] _ _= True
isContained (x:xs) a b = isContained1 x a b && isContained xs a b

isContained1 :: [Int] -> [Int] -> [UMLStateDiagram] -> Bool
isContained1 [] _ _ = True
isContained1 (x:xs) a b =  (x `elem` a)  &&
       isContained1 xs (map label (getSubstate x b)) (getSubstate x b)

getSubstate :: Int -> [UMLStateDiagram] -> [UMLStateDiagram]
getSubstate _ [] =  []
getSubstate a (x:xs) = if a == label x  then
                                           getSubstate1 x
                                        else
                                            getSubstate a xs

getSubstate1 :: UMLStateDiagram -> [UMLStateDiagram]
getSubstate1 (StateDiagram a _ _ _ _) = a
getSubstate1 (CombineDiagram a _) = a
getSubstate1 (Joint _) = []
getSubstate1 (History _ _) = []
getSubstate1 InnerMostState {}  = []


--check local uniqueness
checkUniqueness :: UMLStateDiagram -> Maybe String
checkUniqueness a
  | not(checkSub a) = Just "Error: Local Uniqueness not fullfilled "
  | otherwise = Nothing

checkSub :: UMLStateDiagram -> Bool
checkSub  StateDiagram {substate} =  isUnique (map label substate ) && all checkSub substate
checkSub  CombineDiagram {substate} =  isUnique (map label substate ) && all checkSub substate
checkSub  _ = True

isUnique :: [Int] -> Bool
isUnique a = not (anySame a)



checkStructure :: UMLStateDiagram -> Maybe String
checkStructure a
  | not(checkOuterMostLayer a) = Just ("Error: Outermost layer must be 'StateD"
    ++ "iagram")
  | not(checkSubstateSD a) = Just ("Error: Substate of StateDiagram constructo"
    ++ "r cannot be empty or just History/Joint")
  | not(checkSubstateCD a) = Just ("Error: CombineDiagram constructor must con"
    ++ "tain at least 2 StateDiagram and no other type of constructor")
  | otherwise = Nothing

checkOuterMostLayer :: UMLStateDiagram -> Bool
checkOuterMostLayer Joint {} = False
checkOuterMostLayer History {} = False
checkOuterMostLayer InnerMostState {} = False
checkOuterMostLayer CombineDiagram {} = False
checkOuterMostLayer _ = True

checkSubstateSD :: UMLStateDiagram -> Bool
checkSubstateSD (StateDiagram [] _ _ _ _) = False
checkSubstateSD (CombineDiagram a _) = all checkSubstateSD a
checkSubstateSD (StateDiagram a _ _ _ _) = any checkListInSD a && all checkSubstateSD a
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
