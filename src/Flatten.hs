{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE LambdaCase                #-}
module Flatten (flatten) where

import Datatype (UMLStateDiagram
                ,umlStateDiagram
                ,unUML
                ,StateDiagram(..)
                ,globalise
                ,Connection(..)
                ,rename
                )
import Datatype.ClassInstances ()
import Data.Either.Extra (fromLeft', fromEither)
import Data.List (find)
flatten :: (Num l, Enum l, Eq l, Show l) => UMLStateDiagram n l -> UMLStateDiagram [n] l
flatten
 = maybe (error "not defined") distinctLabels
   . lift
   . fmap Left
   . globalise
   . rename (:[])

lift :: (Eq l) => UMLStateDiagram [n] (Either l l) -> Maybe (UMLStateDiagram [n] (Either l l))
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
                  = map (inheritParentName parentName . rightLabelNodes . fromEither') innerStates
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

rightLabelConnection :: Connection a -> Connection (Either a a)
rightLabelConnection
  = \case
      Connection { pointFrom = pf
                 , pointTo = pt
                 , transition = t }
        -> Connection { pointFrom = map Right pf
                      , pointTo = map Right pt
                      , transition = t }

rightLabelNodes :: StateDiagram n l [Connection l] -> StateDiagram n (Either l l) [Connection (Either l l)]
rightLabelNodes
  = \case
      StateDiagram { label = sdLabel
                   , substates = sdSubstates
                   , startState = sdStartState
                   , connections = sdConnections
                   , name = sdName }
        -> StateDiagram { label = Right sdLabel
                        , substates = map rightLabelNodes sdSubstates
                        , startState = map Right sdStartState
                        , connections = map rightLabelConnection sdConnections
                        , name = sdName }
      CombineDiagram { label = cdLabel
                     , substates = cdSubstates }
        -> CombineDiagram { label = Right cdLabel
                          , substates = map rightLabelNodes cdSubstates }
      EndState { label = esLabel }
        -> EndState { label = Right esLabel }
      ForkOrJoin { label = fjLabel }
        -> ForkOrJoin { label = Right fjLabel }
      History { label = hLabel
              , historyType = hType }
        -> History { label = Right hLabel
                   , historyType = hType }
      InnerMostState { label = imsLabel
                     , name = imsName
                     , operations = imsOperations }
        -> InnerMostState { label = Right imsLabel
                          , name = imsName
                          , operations = imsOperations }

inheritParentName :: [a] -> StateDiagram [a] l c -> StateDiagram [a] l c
inheritParentName pName sd@StateDiagram { name = sdName }
  = sd { name = pName ++ sdName }
inheritParentName pName ims@InnerMostState { name = imsName }
  = ims { name = pName ++ imsName }
inheritParentName _ node = node

fromEither' :: StateDiagram n (Either b b) [Connection (Either b b)] -> StateDiagram n b [Connection b]
fromEither' (StateDiagram { name
                          , label
                          , substates
                          , startState
                          , connections = [] })
  = StateDiagram { name
                 , label = fromEither label
                 , substates = map fromEither' substates
                 , startState = map fromEither startState
                 , connections = [] }
fromEither' (CombineDiagram { label
                              , substates })
  = CombineDiagram { label = fromEither label
                   , substates = map fromEither' substates }
fromEither' (EndState { label })
  = EndState { label = fromEither label }
fromEither' (ForkOrJoin { label })
  = ForkOrJoin { label = fromEither label }
fromEither' (History { label
                       , historyType })
  = History { label = fromEither label
            , historyType }
fromEither' (InnerMostState { name
                            , label
                            , operations})
  = InnerMostState { name
                   , label = fromEither label
                   , operations }
fromEither' _ = error "failed to extract int label"

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
updateCompoundExits address innerExits c@Connection{ pointFrom
                                                   , pointTo
                                                   , transition }
  | pointFrom == [address]
  = [ Connection { pointFrom
                     = [Right label]
                 , pointTo = pointTo
                 , transition = transition
                 } | label <- innerExits ]
  | otherwise = [c]

distinctLabels :: (Eq l, Show l, Num l, Enum l, Eq l, Show l) => UMLStateDiagram a (Either l l) -> UMLStateDiagram a l
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
                        = mapHeadTail (`matchToRelation` r) fromEither startState
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

matchNodeToRelation :: (Eq b, Show b) => [(Either b b, b)] -> StateDiagram n (Either b b) [Connection (Either b b)] -> StateDiagram n b [Connection b]
matchNodeToRelation r
      = \case
           InnerMostState { label, name, operations }
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
                                 = map fromEither' substates
                             , connections
                                 = []
                             , startState
                                 = map fromEither startState }
           CombineDiagram { label
                          , substates
                          }
             -> CombineDiagram { label
                                   = matchToRelation label r
                                , substates
                                    = map fromEither' substates
                                }
           EndState { label }
             -> EndState { label = matchToRelation label r }
           ForkOrJoin { label }
             -> ForkOrJoin { label = matchToRelation label r }
           History { label
                   , historyType }
              -> History { label = matchToRelation label r
                         , historyType = historyType }

mapHeadTail :: (a -> b) -> (a -> b) -> [a] -> [b]
mapHeadTail f g (x:xs) = f x : map g xs
mapHeadTail _ _ _      = error "impossible!"

matchConnectionToRelation :: (Eq b, Show b)
  => [Connection (Either b b)] -> [(Either b b, b)] -> [Connection b]
matchConnectionToRelation connections r
  = [ c { pointFrom
            = mapHeadTail (`matchToRelation` r) fromEither (pointFrom c)
        , pointTo
            = mapHeadTail (`matchToRelation` r) fromEither (pointTo c)
        } | c <- connections ]
