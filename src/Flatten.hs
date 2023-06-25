{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE LambdaCase #-}

module Flatten (
  flatten
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
import Data.List.Extra (replace)

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
       -> error "we dont exepect anything else than StateDiagram or Nothing here"
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

distinctLabels :: FlatDiagram -> FlatDiagram
distinctLabels root@StateDiagram{substate, connection}
  = root { substate
             = zipWith ($) uniqueLabel substate
         , connection
             = foldr (\x y
                       -> (\(u,v)
                            -> (case v of {- v carries the old label -}
                                 InnerMostState {label = oldLabel}
                                   -> (case u v of {- u applied to v yields the new label -}
                                        InnerMostState {label = newLabel}
                                          -> map (\case
                                                    con@Connection { pointFrom
                                                                   , pointTo }
                                                      -> con { pointFrom = replace [oldLabel] [newLabel] pointFrom
                                                             , pointTo = replace [oldLabel] [newLabel] pointTo }
                                                 ) y
                                        _ -> error "not supported"
                                      )
                                 _ -> error "not supported"
                                 )
                           ) x
                     ) connection (zip uniqueLabel substate)
         }
distinctLabels _ = error "we only have one layer and its root must be a StateDiagram"

uniqueLabel :: [FlatDiagram -> FlatDiagram]
uniqueLabel
  = map (\i node
           -> case node of
               InnerMostState{}
                 -> node {label = Left i}
               _ -> error "ensuring unique labels in scenario1 doesnt go beyond 1 layer"
            ) [1..]

type FlatConnection = Connection (Either Int Int)

type FlatDiagram = StateDiagram (Either Int Int) [FlatConnection]
