{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE InstanceSigs              #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Flatten (
   flatten,
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
                     ,Bifunctor(second))

class Flattable a b where
  inFlatForm :: a -> b
  inCompositeForm :: b -> a

type FlatUMLStateDiagram = StateDiagram' [[Int]] [FlatConnection]

type FlatConnection = Connection' [[Int]] [[Int]]

instance Flattable UMLStateDiagram FlatUMLStateDiagram where
  inFlatForm :: UMLStateDiagram -> FlatUMLStateDiagram
  inFlatForm diagram
    = case diagram of
        (StateDiagram {substate, label, name, connection, startState})
          -> StateDiagram { substate = map inFlatForm substate
                         , label = [[label]]
                         , name = name
                         , connection = map inFlatForm connection
                         , startState = startState }
        (InnerMostState {label, name, operations})
          -> InnerMostState { label = [[label]]
                            , name = name
                            , operations = operations }
        (CombineDiagram {substate, label})
          -> CombineDiagram { substate = map inFlatForm substate
                            , label = [[label]] }
        (EndState {label})
          -> EndState { label = [[label]] }
        (Joint {label})
          -> Joint { label = [[label]] }
        (History {label, historyType})
          -> History { label = [[label]]
                     , historyType = historyType }
  inCompositeForm :: FlatUMLStateDiagram -> UMLStateDiagram
  inCompositeForm diagram
    = case diagram of
        (StateDiagram {substate, label, name, connection, startState})
          -> StateDiagram { substate = map inCompositeForm substate
                         , label = sum [sum x|x<-label]
                         , name = name
                         , connection = map inCompositeForm connection
                         , startState = startState }
        (InnerMostState {label, name, operations})
          -> InnerMostState { label = sum [sum x|x<-label]
                            , name = name
                            , operations = operations }
        (CombineDiagram {substate, label})
          -> CombineDiagram { substate = map inCompositeForm substate
                            , label = sum [sum x|x<-label] }
        (EndState {label})
          -> EndState { label = sum [sum x|x<-label] }
        (Joint {label})
          -> Joint { label = sum [sum x|x<-label]}
        (History {label, historyType})
          -> History { label = sum [sum x|x<-label]
                     , historyType = historyType }

instance Flattable Connection FlatConnection where
  inFlatForm :: Connection -> FlatConnection
  inFlatForm (Connection {pointFrom, pointTo, transition })
    = Connection { pointFrom = [pointFrom]
                 , pointTo = [pointTo]
                 , transition = transition}
  inCompositeForm :: FlatConnection -> Connection
  inCompositeForm (Connection {pointFrom, pointTo, transition })
    = Connection { pointFrom = [sum x|x<-pointFrom]
                 , pointTo = [sum x|x<-pointTo]
                 , transition = transition}

flatten :: UMLStateDiagram -> UMLStateDiagram
flatten
  = \case
       s@(StateDiagram {})
         -> head (map inCompositeForm (snd (stomp target)))
            where
            joinFreeConnections
              = (case globalise s of
                   (StateDiagram {connection})
                     -> removeJointConnection' connection s
                   _ -> error "not defined")
            s' = s {connection = joinFreeConnections}
            target = [inFlatForm s'::FlatUMLStateDiagram]
       _ -> error "ill formed diagram, StateDiagram constructor must be root"

stomp :: [FlatUMLStateDiagram] -> ([FlatUMLStateDiagram],[FlatUMLStateDiagram])
stomp [] = ([],[])
stomp [s]
  = case s of
      StateDiagram {substate}
        -> if not $ foldr ((&&) . (\case
                   (InnerMostState {}) -> True
                   _ -> False)) True substate
           then
             case s of
               (StateDiagram { substate = substate'
                    , name = outerName
                    , label = outerLabel })
                -> ([] , [s { substate = snd inheritedParent }])
                   where
                   stomped = flatten'' [s] substate'
                   inheritedParent
                     = second (map
                       (\case
                           ims@(InnerMostState{ name = innerName
                                              , label = innerLabel })
                             -> ims{ name = outerName ++ ", " ++ innerName
                                   , label = map (head outerLabel ++) innerLabel}
                           _ -> error "stomp expects only InnerMostStates internally" ))
                       stomped
           else
           ([s],[s])
      _ -> error "nod defined"

stomp (s:ks)
  = case s of
      (StateDiagram { substate
                    , name = outerName
                    , label = outerLabel
                    , connection
                    , startState })
        -> case inheritedParent of
             (_,z) -> (updatedStack,z)
           where
           stomped = flatten'' (s:ks) substate
           inheritedParent
             = second (map
                 (\case
                     ims@(InnerMostState{ name = innerName
                                        , label = innerLabel })
                       -> ims{ name = outerName ++ ", " ++ innerName
                             , label = map (head outerLabel ++) innerLabel}
                     _ -> error "stomp expects only InnerMostStates internally" ))
               stomped
           liftedConnections
             = map (\case
                       c@Connection {pointFrom,pointTo}
                         -> c { pointFrom = map (head outerLabel ++) pointFrom
                              , pointTo = map (head outerLabel ++) pointTo }) connection
           updatedStack = pushToNextStateDiagram ks liftedConnections
      _ -> error "only StateDiagram can be stomped"

pushToNextStateDiagram :: [FlatUMLStateDiagram] -> [FlatConnection] -> [FlatUMLStateDiagram]
pushToNextStateDiagram (s@(StateDiagram{connection}):xs) con
  = s {connection = connection ++ con} : pushToNextStateDiagram xs []
pushToNextStateDiagram (i:xs) con
  = i : pushToNextStateDiagram xs con
pushToNextStateDiagram [] _ = []

-- rewireStompedTransitions :: ([UMLStateDiagram],[UMLStateDiagram]) -> ([UMLStateDiagram],[UMLStateDiagram])
-- rewireStompedTransitions


cross :: [FlatUMLStateDiagram] -> ([FlatUMLStateDiagram],[FlatUMLStateDiagram])
cross (s:ks)
  = case s of
      (CombineDiagram {substate})
        -> crossedInnerTransitions
           where
           multiStomped = flatten''' (s:ks) (map (: []) substate)
           crossed = second crossProduct multiStomped {- do transition updates recursively along the structure here -}
           crossedInnerTransitions = crossInnerTransitions crossed
      _ -> error "only CombineDiagram can be crossed"
cross [] = ([],[])

crossInnerTransitions :: ([FlatUMLStateDiagram],[FlatUMLStateDiagram]) -> ([FlatUMLStateDiagram],[FlatUMLStateDiagram])
crossInnerTransitions (sk,states)
  = (sk,states)

flatten' :: [FlatUMLStateDiagram] -> ([FlatUMLStateDiagram],[FlatUMLStateDiagram])
flatten' [] = ([],[])
flatten' (s:ks)
  = case s of
      (StateDiagram {})
        -> stomp (s:ks)
      (CombineDiagram {})
        -> cross (s:ks)
      ims@(InnerMostState {})
        -> (ks,[ims])
      (EndState {})
        -> error "([],[]) and remove while rewire"
      (Joint {})
        -> ([],[])
          -- TODO: purge Joint constructors from structure
          -- error "any Joint connections need to be converted and removed before flattening"
      (History {})
        -> error "not supported"

flatten'' :: [FlatUMLStateDiagram] -> [FlatUMLStateDiagram] -> ([FlatUMLStateDiagram],[FlatUMLStateDiagram])
flatten'' sk [] = (sk,[])
flatten'' sk (sub:substate)
  = let
    cur = flatten' (sub:sk)
    next = flatten'' (fst cur) substate
    in
    bimap ([] ++) (snd cur ++) next

flatten''' :: [FlatUMLStateDiagram] -> [[FlatUMLStateDiagram]] -> ([FlatUMLStateDiagram],[[FlatUMLStateDiagram]])
flatten''' sk [] = (sk,[])
flatten''' sk (sbl:substateLists)
  = let
    cur = flatten'' sk sbl
    next = flatten''' (fst cur) substateLists
    in
    bimap ([] ++) (snd cur :) next

crossStates :: FlatUMLStateDiagram -> FlatUMLStateDiagram -> FlatUMLStateDiagram
crossStates (InnerMostState { label = leftLabel
                            , name = leftName
                            , operations = leftOperations })
            (InnerMostState { label = rightLabel
                            , name = rightName
                            , operations = rightOperations  })
  = InnerMostState { label = leftLabel ++ rightLabel
                   , name = leftName ++ ", " ++ rightName
                   , operations = leftOperations ++ "\n" ++ rightOperations }
crossStates _ _
  = error "only states that decayed into an InnerMostState can be crossed"

crossProduct' :: [FlatUMLStateDiagram] -> [FlatUMLStateDiagram] -> [FlatUMLStateDiagram]
crossProduct' xs ys
  = [ crossStates x y | x <- xs, y <-ys ]

crossProduct :: [[FlatUMLStateDiagram]] -> [FlatUMLStateDiagram]
crossProduct [] = []
crossProduct [x] = x
crossProduct (x:y:xs) = crossProduct (crossProduct' x y : xs)

getLabel :: StateDiagram a -> Int
getLabel = \case
              (StateDiagram {label}) -> label
              (CombineDiagram {label}) -> label
              (InnerMostState {label}) -> label
              (Joint {label}) -> label
              (History {label}) -> label
              (EndState {label}) -> label

walkTo :: [Int] -> UMLStateDiagram -> UMLStateDiagram
walkTo xs diagram = foldl (flip followLabel) diagram xs

followLabel :: Int -> UMLStateDiagram -> UMLStateDiagram
followLabel i j
  = head $ filter (\case y -> getLabel y == i) substates
  where
  substates = (\case
        (StateDiagram {substate}) -> substate
        (CombineDiagram {substate}) -> substate
        _ -> []) j


replaceMatching :: [[Int]] -> [([[Int]],[[Int]])] -> [[Int]]
replaceMatching [] _ = []
replaceMatching (x:xs) ys
  = if null possibleTarget then x : replaceMatching xs ys
    else possibleTarget ++ replaceMatching xs ys
    where
    possibleTarget = [t | (y, y') <- ys, s <- y, sort x == sort s, t <- y']

{-

asSourceToTarget :: [FlatConnection] -> [([[Int]],[[Int]])]
asSourceToTarget [] = []
asSourceToTarget ((Connection {pointTo,pointFrom}):xs) = (pointFrom,pointTo):asSourceToTarget xs

groupSameConnections :: [FlatConnection] -> [[FlatConnection]]
groupSameConnections = groupBy transitionName' . sortBy transitionName

transitionName :: FlatConnection -> FlatConnection -> Ordering
transitionName (Connection{transition}) (Connection{transition=transition'})
    | transition >= transition' = GT
    | otherwise = LT

transitionName' :: FlatConnection -> FlatConnection -> Bool
transitionName' (Connection{transition}) (Connection{transition=transition'})
    = transition == transition'
-}

removeJointConnection' :: [Connection] -> UMLStateDiagram -> [Connection]
removeJointConnection' connection diagram
  = [c|c@(Connection{}) <- connection
     , null $ conToJoint [c] diagram
     , null $ conFromJoint [c] diagram ] -- hlinter breaks after this line for some reason

conToJoint :: [Connection] -> UMLStateDiagram -> [Connection]
conToJoint con diag = [c|c@Connection{pointTo}<-con
            , (\case
                  (Joint {}) -> True
                  _ -> False)
             (walkTo pointTo diag) ]

conFromJoint :: [Connection] -> UMLStateDiagram -> [Connection]
conFromJoint con diag = [c|c@Connection{pointFrom}<-con
            , (\case
                  (Joint {}) -> True
                  _ -> False)
              (walkTo pointFrom diag) ]
