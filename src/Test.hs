module Test where
import Datatype
import Layout

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
