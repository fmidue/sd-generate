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
                ,rename
                )
import Datatype.ClassInstances ()
import Data.Either.Extra (fromLeft'
                         )
import Data.List (find)

flatten :: (Num l, Enum l, Eq l, Show l)
  => UMLStateDiagram n l -> UMLStateDiagram [n] l
flatten
 = maybe (error "not defined") distinctLabels
   . lift
   . fmap Left
   . globalise
   . rename (:[])

lift :: (Eq l)
  => UMLStateDiagram [n] (Either l l) -> Maybe (UMLStateDiagram [n] (Either l l))
lift
  = fmap umlStateDiagram . unUML
    (\outerName outerStates connections outerStartState ->
      case find (\case
                   StateDiagram {}
                     -> True
                   _ -> False) outerStates
      of
     Just StateDiagram { label = address
                       , startState = innerStartState
                       , substates = innerStates
                       , name = parentName }
       -> let
          initial
            = map (Right . fromLeft') innerStartState
          in
          Just (
            StateDiagram
              { name
                  = outerName
              , startState
                  = updateByRule address initial outerStartState
              , label
                  = undefined
              , substates
                  = map (\case
                           i@InnerMostState{ name = innerName
                                           , label = Left innerLabel }
                             -> i { name = parentName ++ innerName
                                  , label = Right innerLabel }
                           _ -> error "scenario1 only expects InnerMostStates as substates of a StateDiagram"
                        ) innerStates
                    ++
                    filter ((address /=) . label) outerStates
              , connections
                  = rewire connections address initial
                      (map (fromLeft' . label) innerStates)
              }
               )
     Just _
       -> error "we dont expect anything else than StateDiagram or Nothing here"
     Nothing
       -> Nothing
    )

rewire :: Eq l
  => [Connection (Either l l)] -> Either l l -> [Either l l] -> [l] -> [Connection (Either l l)]
rewire connections address initial innerExits
  = map (updateLifted address initial) $
    concatMap (updateCompoundExits address innerExits) connections

updateByRule :: Eq l
  => Either l l -> [Either l l] -> [Either l l] -> [Either l l]
updateByRule address initial [x]
  | x == address = initial
updateByRule address _ (x:xs)
  | x == address = map (Right . fromLeft') xs
updateByRule _ _ labels = labels

updateLifted :: Eq l
  => Either l l -> [Either l l] -> Connection (Either l l) -> Connection (Either l l)
updateLifted address initial c@(Connection{pointFrom,pointTo})
  = c { pointFrom = updateByRule address initial pointFrom
      , pointTo = updateByRule address initial pointTo }

updateCompoundExits :: (Eq l, Eq r)
  => Either l r -> [r] -> Connection (Either l r) -> [Connection (Either l r)]
updateCompoundExits address innerExits
                                  c@Connection{ pointFrom
                                              , pointTo
                                              , transition }
  | pointFrom == [address]
  = [ Connection { pointFrom
                     = [Right label]
                 , pointTo = pointTo
                 , transition = transition
                 } | label <- innerExits ]
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
                        = map (matchNodeToRelation r) substates
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

matchNodeToRelation :: (Eq c, Eq b, Show c, Show b)
  => [(Either b c, b)] -> StateDiagram n (Either b c) [Connection (Either b c)] -> StateDiagram n b [Connection b]
matchNodeToRelation r
      = (\case
           InnerMostState{ label, name, operations }
             -> InnerMostState { label
                                   = matchToRelation label r
                                , name = name
                                , operations = operations
                               }
           StateDiagram { label
                        , substates
                        , name
                        , startState
                        }
             -> StateDiagram { label
                                 = matchToRelation label r
                             , name
                                 = name
                             , substates
                                 = map (\case
                                          InnerMostState { label
                                                             = Left x
                                                         , name = innerName
                                                         , operations }
                                            -> InnerMostState { label
                                                                  = x
                                                              , name
                                                                  = innerName
                                                              , operations
                                                                  = operations }
                                          _ -> error "requires more sophisticated traversal"
                                       ) substates
                             , connections
                                 = []
                             , startState
                                 = map fromLeft' startState }
           _ -> error "not covered constructor to match relation against")

mapHeadTail :: (a -> b) -> (a -> b) -> [a] -> [b]
mapHeadTail f g (x:xs) = f x : map g xs
mapHeadTail _ _ _      = error "impossible!"

matchConnectionToRelation :: (Eq b, Eq c, Show b, Show c)
  => [Connection (Either b c)] -> [(Either b c, b)] -> [Connection b]
matchConnectionToRelation connections r
  = [ c { pointFrom
            = mapHeadTail (`matchToRelation` r) fromLeft' (pointFrom c)
        , pointTo
            = mapHeadTail (`matchToRelation` r) fromLeft' (pointTo c)
        } | c <- connections ]
