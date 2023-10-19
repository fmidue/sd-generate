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
import Data.Either.Extra (fromLeft'
                         ,fromEither)
import Data.List (find)
import Data.Bifunctor (bimap)

flatten :: (Eq l, Enum l, Num l) => UMLStateDiagram n l -> UMLStateDiagram [n] l
flatten
 = maybe (error "not defined") distinctLabels
   . liftSD
   . fmap Left
   . globalise
   . rename (:[])

flatten' :: (Eq l, Enum l, Num l) => UMLStateDiagram [n] l -> UMLStateDiagram [n] l
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
              { name = outerName
              , startState = updateByRule address initial outerStartState
              , label = undefined
              , substates
                  = map
                    (inheritParentName parentName . bimap (Right . fromLeft') (map (fmap (Right . fromLeft'))))
                    innerStates
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

inheritParentName :: [n] -> StateDiagram [n] l c -> StateDiagram [n] l c
inheritParentName pName sd@StateDiagram { name = sdName }
  = sd { name = pName ++ sdName }
inheritParentName pName ims@InnerMostState { name = imsName }
  = ims { name = pName ++ imsName }
inheritParentName _ node = node

rewire :: (Eq l, Foldable c) => c (Connection (Either l l)) -> Either l l -> [Either l l] -> [l] -> [Connection (Either l l)]
rewire connections address initial innerExits
  = map (updateLifted address initial) $
    concatMap (updateCompoundExits address innerExits) connections

updateByRule :: (Eq l) => Either l l -> [Either l l] -> [Either l l] -> [Either l l]
updateByRule address initial [x]
  | x == address = initial
updateByRule address _ (x:xs)
  | x == address = map (Right . fromLeft') xs
updateByRule _ _ labels = labels

updateLifted :: (Eq l) => Either l l -> [Either l l] -> Connection (Either l l) -> Connection (Either l l)
updateLifted address initial c@(Connection{pointFrom,pointTo})
  = c { pointFrom = updateByRule address initial pointFrom
      , pointTo = updateByRule address initial pointTo }

updateCompoundExits :: (Eq l) => Either l l -> [l] -> Connection (Either l l) -> [Connection (Either l l)]
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


distinctLabels :: (Eq l, Enum l, Num l) => UMLStateDiagram n (Either l l) -> UMLStateDiagram n l
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


matchToRelation :: Eq a => a -> [(a, b)] -> b
matchToRelation x r
  = case lookup x r of
     Just u
       -> u
     Nothing
       -> error "no matching label can be found"

matchNodeToRelation :: (Eq l, Functor f) => [(Either l l, l)] -> StateDiagram n (Either l l) [f (Either b b)] -> StateDiagram n l [f b]
matchNodeToRelation r
      = \case
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
           s -> let
                l = label s
                in
                (bimap fromEither (map $ fmap fromEither) s) { label = matchToRelation l r }

mapHeadTail :: (a -> b) -> (a -> b) -> [a] -> [b]
mapHeadTail f g (x:xs) = f x : map g xs
mapHeadTail _ _ _      = error "impossible!"

matchConnectionToRelation :: (Eq l) => [Connection (Either l l)] -> [(Either l l, l)] -> [Connection l]
matchConnectionToRelation connections r
  = [ c { pointFrom
            = mapHeadTail (`matchToRelation` r) fromEither (pointFrom c)
        , pointTo
            = mapHeadTail (`matchToRelation` r) fromEither (pointTo c)
        } | c <- connections ]

