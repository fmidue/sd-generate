{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE LambdaCase #-}

module Flatten (
  flatten
 ,FlatDiagram
 ,FlatConnection
) where
import Datatype (UMLStateDiagram
                ,umlStateDiagram
                ,unUML
                ,StateDiagram(..)
                ,globalise
                ,Connection(..)
                ,rename
                )
import Datatype.ClassInstances ()
import Data.Either.Extra (fromLeft'
                         ,fromRight'
                         )
import Data.List (find)

flatten :: UMLStateDiagram n Int -> UMLStateDiagram [n] Int
flatten
 = distinctLabels
   . lift
   . fmap Left
   . globalise
   . rename (\n -> [n])

lift :: UMLStateDiagram [n] (Either Int Int) -> UMLStateDiagram [n] (Either Int Int)
lift
  = umlStateDiagram . unUML
    (\name substate connection outerStartState ->
    case target substate of
     Just StateDiagram { label
                       , startState
                       , substate = inner
                       , name = parentName }
       -> let
          address = label
          initial
            = map (Right . fromLeft') startState
          in
          StateDiagram
            { name = name
            , startState = outerStartState
            , label = undefined
            , substate
                = map (\case
                         i@InnerMostState{ name = innerName
                                         , label = Left innerLabel }
                           -> i { name = parentName ++ innerName
                                , label = Right innerLabel }
                         _ -> error "scenario1 only expects InnerMostStates as substates of a StateDiagram"
                      ) inner
                  ++
                  filter (\case
                            InnerMostState {}
                              -> True
                            _ -> False
                         ) substate
            , connection = rewire connection address initial inner }
     Just _
       -> error "we dont expect anything else than StateDiagram or Nothing here"
     Nothing
       -> error "scenario1 expects at least one hierarchical state")

target :: [FlatDiagram a] -> Maybe (FlatDiagram a)
target = find (\case
                 StateDiagram {}
                   -> True
                 _ -> False)

rewire :: [FlatConnection] -> Either Int Int -> [Either Int Int] -> [FlatDiagram a] -> [FlatConnection]
rewire connections address initial inner
  = map (updateLifted address initial) $
    concatMap (updateCompoundExits address inner) connections

updateByRule :: Either Int Int -> [Either Int Int] -> [Either Int Int] -> [Either Int Int]
updateByRule address initial [x]
  | x == address = map (Right . fromRight') initial
updateByRule address _ (x:xs)
  | x == address = map (Right . fromLeft') xs
updateByRule _ _ labels = labels

updateLifted :: Either Int Int -> [Either Int Int] -> FlatConnection -> FlatConnection
updateLifted address initial c@(Connection{pointFrom,pointTo})
  = c { pointFrom = updateByRule address initial pointFrom
      , pointTo = updateByRule address initial pointTo }

updateCompoundExits :: Either Int Int -> [FlatDiagram a] -> FlatConnection -> [FlatConnection]
updateCompoundExits address inner c@Connection{ pointFrom
                                              , pointTo
                                              , transition }
  | pointFrom == [address]
  = [ Connection { pointFrom
                     = [(Right . fromLeft') label]
                 , pointTo = pointTo
                 , transition = transition
                 } | InnerMostState{label} <- inner ]
  | otherwise = [c]

distinctLabels :: UMLStateDiagram a (Either Int Int) -> UMLStateDiagram a Int
distinctLabels
  = umlStateDiagram . unUML
    (\name substate connection startState ->
       StateDiagram { substate
                        = matchNodesToRelation substate
                          (eitherLabelToLeftRelation substate)
                    , connection
                        = matchConnectionToRelation connection
                          (eitherLabelToLeftRelation substate)
                    , name = name
                    , startState = map (\x -> matchToRelation x (eitherLabelToLeftRelation substate)) startState
                    , label = error "not relevant"
                    }
    )

matchToRelation :: (Foldable t, Eq a) => a -> t (a, b) -> b
matchToRelation x r
  = case find (\(old,_) -> [old] == [x]) r of
     Just (_,u)
       -> u
     Nothing
       -> error "no matching label can be found for update"

matchNodesToRelation :: (Eq a) => [StateDiagram n a [Connection a]] -> [(a, b)] -> [StateDiagram n b [Connection b]]
matchNodesToRelation substate r
  = map (\case
           InnerMostState{ label, name, operations }
             -> InnerMostState { label
                                   = matchToRelation label r
                                , name = name
                                , operations = operations
                               }
           _ -> error "only InnerMostStates are allowed at this point")
    substate

matchConnectionToRelation :: Eq a => [Connection a] -> [(a, b)] -> [Connection b]
matchConnectionToRelation connection r
  = [ c { pointFrom = map  (`matchToRelation` r) (pointFrom c)
        , pointTo = map (`matchToRelation` r) (pointTo c)
        } |c<-connection ]

eitherLabelToLeftRelation :: [StateDiagram n (Either a b) [Connection (Either a b)]] -> [(Either a b, Int)]
eitherLabelToLeftRelation substate
  = zip
    (map label substate)
    [1..]


type FlatConnection = Connection (Either Int Int)

type FlatDiagram n = StateDiagram n (Either Int Int) [FlatConnection]
