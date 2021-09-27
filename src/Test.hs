{-# LANGUAGE NamedFieldPuns #-}
module Test where
import Datatype
import Layout
import Data.List.Extra

--check semantics
checkSemantics :: UMLStateDiagram -> Maybe String
checkSemantics a
  | not(checkOutermostHistory a) = Just "Error: History does not make sense in the outermost stateDiagram "
  | not(checkEndOutEdges a) = Just "Error: no EndState should have outgoing edges"
  | otherwise = Nothing

                --checkOutermostHistory
checkOutermostHistory :: UMLStateDiagram -> Bool
checkOutermostHistory (StateDiagram a _ _ _ _) = all isHistoryNotInSD a
checkOutermostHistory _ = True

isHistoryNotInSD :: UMLStateDiagram -> Bool
isHistoryNotInSD History {} = False
isHistoryNotInSD _ = True

                --checkEndOutEdges
checkEndOutEdges :: UMLStateDiagram -> Bool
checkEndOutEdges StateDiagram { substate, connection } = all ((`isConnFromNotEnd` substate) . pointFrom) connection && all checkEndOutEdges substate
checkEndOutEdges CombineDiagram {substate} = all checkEndOutEdges substate
checkEndOutEdges  _ = True

isConnFromNotEnd :: [Int] -> [UMLStateDiagram] -> Bool
isConnFromNotEnd [] _ = True
isConnFromNotEnd [x] a = all (isNotEnd x) a
isConnFromNotEnd (x:xs) a = isConnFromNotEnd xs (getSubstate x a)

isNotEnd :: Int -> UMLStateDiagram -> Bool
isNotEnd a EndState {label}  = a /= label
isNotEnd _ _ = True

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
isContained xs a b = all (\x -> isContained1 x a b) xs

isContained1 :: [Int] -> [Int] -> [UMLStateDiagram] -> Bool
isContained1 [] _ _ = True
isContained1 (x:xs) a b =  (x `elem` a)  &&
       isContained1 xs (map label (getSubstate x b)) (getSubstate x b)

getSubstate :: Int -> [UMLStateDiagram] -> [UMLStateDiagram]
getSubstate a xs = maybe [] getSubstate1 (find ((a ==) . label) xs)

getSubstate1 :: UMLStateDiagram -> [UMLStateDiagram]
getSubstate1 (StateDiagram a _ _ _ _) = a
getSubstate1 (CombineDiagram a _) = a
getSubstate1 _ = []


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

--checkStructure
checkStructure :: UMLStateDiagram -> Maybe String
checkStructure a
  | not(checkOuterMostLayer a) = Just ("Error: Outermost layer must be 'StateD"
    ++ "iagram")
  | not(checkSubstateSD a) = Just ("Error: Substate of StateDiagram constructo"
    ++ "r cannot be empty or just History/Joint")
  | not(checkSubstateCD a) = Just ("Error: CombineDiagram constructor must con"
    ++ "tain at least 2 StateDiagram and no other type of constructor")
  | not(checkHistOutTransition a) = Just "Error: Outgoing edges from a history node always have the empty transition"
  | not(checkSameConnection a) = Just "Error: No two connections are allowed leaving the same source and and having the same label."
  | otherwise = Nothing

                --checkOuterMostLayer
checkOuterMostLayer :: UMLStateDiagram -> Bool
checkOuterMostLayer StateDiagram{} = True
checkOuterMostLayer _ = False

                --checkSubstateSD
checkSubstateSD :: UMLStateDiagram -> Bool
checkSubstateSD (StateDiagram [] _ _ _ _) = False
checkSubstateSD (CombineDiagram a _) = all checkSubstateSD a
checkSubstateSD (StateDiagram a _ _ _ _) = any checkListInSD a && all checkSubstateSD a
checkSubstateSD _ = True

checkListInSD :: UMLStateDiagram -> Bool
checkListInSD Joint {} = False
checkListInSD History {} = False
checkListInSD _ = True


                --checkSubstateCD
checkSubstateCD :: UMLStateDiagram -> Bool
checkSubstateCD (CombineDiagram [] _) = False
checkSubstateCD (CombineDiagram [_] _) = False
checkSubstateCD (CombineDiagram a _) = all checkListInCD a
checkSubstateCD (StateDiagram a _ _ _ _) = all checkSubstateCD a
checkSubstateCD _ = True

checkListInCD :: UMLStateDiagram -> Bool
checkListInCD (StateDiagram a _ _ _ _) = all checkSubstateCD a
checkListInCD _ = False
                 --checkHistOutTransitions
checkHistOutTransition :: UMLStateDiagram -> Bool
checkHistOutTransition StateDiagram { substate, connection } = all (`checkHistConnTransition` substate) connection && all checkHistOutTransition substate
checkHistOutTransition CombineDiagram {substate} = all checkHistOutTransition substate
checkHistOutTransition  _ = True

checkHistConnTransition :: Connection -> [UMLStateDiagram] -> Bool
checkHistConnTransition Connection { pointFrom,transition } a = isConnFromNotHistory pointFrom a || strEmpty transition

strEmpty :: String -> Bool
strEmpty [] = True
strEmpty _ = False

isConnFromNotHistory :: [Int] -> [UMLStateDiagram] -> Bool
isConnFromNotHistory [] _ = True
isConnFromNotHistory [x] a = all (isNotHistory x) a
isConnFromNotHistory (x:xs) a = isConnFromNotHistory xs (getSubstate x a)

isNotHistory :: Int -> UMLStateDiagram -> Bool
isNotHistory a History {label}  = a /= label
isNotHistory _ _ = True

                --checkSameConnection
checkSameConnection :: UMLStateDiagram -> Bool
checkSameConnection a  = not (anySame (filter tranNotEmpty (getConnectionList a [])))

tranNotEmpty :: ([Int],String) -> Bool
tranNotEmpty (_,[]) = False
tranNotEmpty (_,_) = True

getConnectionList :: UMLStateDiagram -> [Int] -> [([Int],String)]
getConnectionList StateDiagram { substate, connection ,label} a = map (\x -> getConnection x (a ++ [label])) connection
                                                           ++ concatMap (\ x -> getConnectionList x (a ++ [label])) substate
getConnectionList CombineDiagram {substate,label} a = concatMap (\ x -> getConnectionList x (a ++ [label])) substate
getConnectionList  _ _ =[]

getConnection :: Connection -> [Int] -> ([Int],String)
getConnection Connection{pointFrom,transition} a =  (from,tran)
                                              where from = a ++ pointFrom
                                                    tran  = transition

--checkWrapper
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
