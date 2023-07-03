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
import Data.List (find, isPrefixOf)

flatten :: (Num l, Enum l, Eq l, Show l)
  => UMLStateDiagram n l -> UMLStateDiagram [n] l
flatten x
 = (\case
      Just y -> distinctLabels y
      Nothing -> error "not defined")
   (lift
   $ fmap Left
   $ globalise
   $ rename (:[]) x)

lift :: Eq l
  => UMLStateDiagram [n] (Either l l) -> Maybe (UMLStateDiagram [n] (Either l l))
lift
  = unUML (\name substates connections outerStartState ->
      case find (\case
                   StateDiagram {}
                     -> True
                   _ -> False) substates
      of
     Just StateDiagram { label
                       , startState
                       , substates = inner
                       , name = parentName }
       -> let
          address = label
          initial
            = map (Right . fromLeft') startState
          in
          Just (
           umlStateDiagram
            StateDiagram
              { name = name
              , startState
                  = if [label] `isPrefixOf` outerStartState
                    then if null $ tail outerStartState
                         then initial
                         else map (Right . fromLeft') $ tail outerStartState
                    else outerStartState
              , label
                  = undefined
              , substates
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
                           ) substates
              , connections
                  = rewire connections address initial inner }
               )
     Just _
       -> error "we dont expect anything else than StateDiagram or Nothing here"
     Nothing
       -> Nothing
    )


rewire :: Eq l
  => [FlatConnection l] -> Either l l -> [Either l l] -> [FlatDiagram a l] -> [FlatConnection l]
rewire theConnections address initial inner
  = map (updateLifted address initial) $
    concatMap (updateCompoundExits address inner) theConnections

updateByRule :: Eq l
  => Either l l -> [Either l l] -> [Either l l] -> [Either l l]
updateByRule address initial [x]
  | x == address = map (Right . fromRight') initial
updateByRule address _ (x:xs)
  | x == address = map (Right . fromLeft') xs
updateByRule _ _ labels = labels

updateLifted :: Eq l
  => Either l l -> [Either l l] -> FlatConnection l -> FlatConnection l
updateLifted address initial c@(Connection{pointFrom,pointTo})
  = c { pointFrom = updateByRule address initial pointFrom
      , pointTo = updateByRule address initial pointTo }

updateCompoundExits :: Eq l
  => Either l l -> [FlatDiagram a l] -> FlatConnection l -> [FlatConnection l]
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

distinctLabels :: (Eq b, Show b, Num l, Enum l, Eq l, Show l)
  => UMLStateDiagram a (Either l b) -> UMLStateDiagram a l
distinctLabels
  = umlStateDiagram . unUML
    (\name substates connections startState ->
       let
       r = zip
           (map label substates)
           [1..]
       in
       StateDiagram { substates
                        = matchNodesToRelation substates r
                    , connections
                        = matchConnectionToRelation connections r
                    , name = name
                    , startState
                        = map (`matchToRelation` r) startState
                    , label = error "not relevant"
                    }
    )

matchToRelation :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
matchToRelation x r
  = case lookup x r of
     Just u
       -> u
     Nothing
       -> error $ "no matching label can be found for " ++ show x ++ " while updating using " ++ show r

matchNodesToRelation :: (Eq c, Eq b, Show c, Show b)
  => [StateDiagram n (Either b c) [Connection (Either b c)]] -> [(Either b c, b)] -> [StateDiagram n b [Connection b]]
matchNodesToRelation substates r
  = map (\case
           InnerMostState{ label, name, operations }
             -> InnerMostState { label
                                   = matchToRelation label r
                                , name = name
                                , operations = operations
                               }
           _ -> error "only InnerMostStates are allowed at this point")
    substates

matchConnectionToRelation :: (Eq b, Eq c, Show b, Show c)
  => [Connection (Either b c)] -> [(Either b c, b)] -> [Connection b]
matchConnectionToRelation connections r
  = [ c { pointFrom = map  (`matchToRelation` r) (pointFrom c)
        , pointTo = map (`matchToRelation` r) (pointTo c)
        } | c <- connections ]


type FlatConnection l = Connection (Either l l)

type FlatDiagram n l = StateDiagram n (Either l l) [FlatConnection l]
