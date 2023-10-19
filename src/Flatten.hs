{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE LambdaCase                #-}
module Flatten (flatten
               ,flatten') where

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
import Data.Bifunctor (bimap)

flatten :: (Num l, Enum l, Eq l, Show l) => UMLStateDiagram n l -> UMLStateDiagram [n] l
flatten
 = maybe (error "not defined") distinctLabels
   . liftSD
   . fmap Left
   . globalise
   . rename (:[])

flatten' :: (Num l, Enum l, Eq l, Show l) => UMLStateDiagram [n] l -> UMLStateDiagram [n] l
flatten'
 = maybe (error "not defined") distinctLabels
   . liftSD
   . fmap Left
   . globalise

liftSD :: (Eq l) => UMLStateDiagram [n] (Either l l) -> Maybe (UMLStateDiagram [n] (Either l l))
liftSD
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
          initial = map (Right . fromLeft') innerStartState
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
                  = map (inheritParentName parentName . bimap (Right . fromEither) (map (fmap (Right . fromEither)))) innerStates
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

inheritParentName :: [a] -> StateDiagram [a] l c -> StateDiagram [a] l c
inheritParentName pName sd@StateDiagram { name = sdName }
  = sd { name = pName ++ sdName }
inheritParentName pName ims@InnerMostState { name = imsName }
  = ims { name = pName ++ imsName }
inheritParentName _ node = node

{-
fromEither' :: StateDiagram n (Either b b) [Connection (Either b b)] -> StateDiagram n b [Connection b]
fromEither'
  = bimap fromEither (map $ fmap fromEither)
    -- first (fromEither) . second (map $ fmap fromEither)
-}

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

updateCompoundExits :: (Eq l, Eq r) => Either l r -> [r] -> Connection (Either l r) -> [Connection (Either l r)]
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
                                 = map (bimap fromEither (map $ fmap fromEither)) substates
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
                                    = map (bimap fromEither (map $ fmap fromEither)) substates
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

