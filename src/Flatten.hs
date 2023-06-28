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
                )
import Datatype.ClassInstances ()
import Data.Either.Extra (fromLeft'
                         --,mapRight
                         )
import Data.List (find)

flatten :: UMLStateDiagram Int -> UMLStateDiagram Int
flatten
 = fmap (either id id)
  . umlStateDiagram
  . distinctLabels
  . unUML lift
  . fmap Left
  . globalise
   where
   lift name substate connection outerStartState =
    case target substate of
     Just StateDiagram { label
                       , startState
                       , substate = inner }
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
                           -> i { name = name ++ "_" ++ innerName
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
       -> error "scenario1 expects at least one hierarchical state"

target :: [FlatDiagram] -> Maybe FlatDiagram
target substate
         = let
           sd = filter (\case
                   StateDiagram {}
                     -> True
                   _ -> False ) substate
           in
           if not (null sd)
           then Just (head sd)
           else Nothing

rewire :: [FlatConnection] -> Either Int Int -> [Either Int Int] -> [FlatDiagram] -> [FlatConnection]
rewire connections address initial inner
  = map (updateLifted address initial) $
    concatMap (updateCompoundExits address inner) connections

updateByRule :: Either Int Int -> [Either Int Int] -> [Either Int Int] -> [Either Int Int]
updateByRule address initial [x]
  | x == address = map (\case
                          Left y -> Right y
                          Right y -> Right y) initial
updateByRule address _ (x:xs)
  | x == address = map (Right . fromLeft') xs
updateByRule _ _ labels = labels

updateLifted :: Either Int Int -> [Either Int Int] -> FlatConnection -> FlatConnection
updateLifted address initial c@(Connection{pointFrom,pointTo})
  = c { pointFrom = updateByRule address initial pointFrom
      , pointTo = updateByRule address initial pointTo }

updateCompoundExits :: Either Int Int -> [FlatDiagram] -> FlatConnection -> [FlatConnection]
updateCompoundExits address inner c@Connection{ pointFrom
                                              , pointTo
                                              , transition }
  | pointFrom == [address]
  = [ Connection { pointFrom
                     = [(\case
                           Left y -> Right y
                           _ -> error "input should be Left" ) label]
                 , pointTo = pointTo
                 , transition = transition
                 } | InnerMostState{label} <- inner ]
  | otherwise = [c]

{- makes the labels distinct on one layer, i.e. adjusts present substates and connections accordingly
   TODO: must also touch startState (it doesnt right now) -}
distinctLabels :: FlatDiagram -> FlatDiagram
distinctLabels root@StateDiagram{substate, connection}
  = root { substate
             = matchNodesToRelation substate
               (mixedLabelToFullLeftRelation substate)
         , connection
             = matchConnectionToRelation connection
               (mixedLabelToFullLeftRelation substate)
         }
distinctLabels _
  = error "we only have one layer and its root must be a StateDiagram"

{- replaces labels according to a mapping provided from a relation of change -}
matchNodesToRelation :: Eq a1 => [StateDiagram a1 a2] -> [(a1, a1)] -> [StateDiagram a1 a2]
matchNodesToRelation substate r
  = map (\case
           inner@InnerMostState{label}
             -> inner { label
                          = case find (\(old,_) -> [old] == [label]) r of
                             Just (_,u)
                               -> u
                             Nothing
                               -> error "no matching node label can be found for update"
                      }
           _ -> error "only InnerMostStates are allowed at this point")
    substate

{- replaces labels used within connections according to a mapping provided from a relation of change
   , this function was isolated to be used in testing -}
matchConnectionToRelation :: Eq a => [Connection a] -> [(a, label)] -> [Connection label]
matchConnectionToRelation connection r
  = [ c { pointFrom = replace (pointFrom c)
        , pointTo = replace (pointTo c)
        } |c<-connection ]
    where
    replace x = case find (\(old,_) -> [old] == x) r of
                  Just (_,u)
                    -> [u]
                  Nothing
                    -> error "no matching connection label can be found for update"

{- builds a relation; which is a list of tuples, wherein the old mixed labels of substates are mapped towards new Left labels  -}
mixedLabelToFullLeftRelation :: [StateDiagram (Either Int b) a] -> [(Either Int b, Either Int b)]
mixedLabelToFullLeftRelation substate
  = zip
    (map label substate)
    (map Left [1..])


type FlatConnection = Connection (Either Int Int)

type FlatDiagram = StateDiagram (Either Int Int) [FlatConnection]
