{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RecordWildCards           #-}

module Datatype
  ( Wrapper(..)
  , ConnectWithType(..)
  , Connection(..)
  , ConnectionType(..)
  , HistoryType(..)
  , UMLStateDiagram(unUML')
  , umlStateDiagram
  , unUML
  , Layout(..)
  , RightConnect(..)
  , StateDiagram(..)
  , globalise
  , localise
  , hoistOutwards
  , rename
  ) where

import GHC.Generics (Generic)

import Data.Bifunctor.TH (
  deriveBifoldable,
  deriveBifunctor,
  deriveBitraversable,
  )
import Data.List                        (partition)

data Wrapper =
  OrDecomposition { layered :: [[Wrapper]],
            key :: Int,
            strings :: String,
            connections :: [ConnectWithType],
            layout :: Layout,
            maxLabel :: Int,
            lengthXY :: Double,
            rightC :: RightConnect,
            outerLayout :: Layout
          }
  | AndDecomposition { component :: [Wrapper],
               key :: Int,
               layout :: Layout,
               lengthXY :: Double,
               rightC :: RightConnect,
               outerLayout :: Layout
             }
  | EndS {
      key         :: Int,
      lengthXY    :: Double,
      rightC      :: RightConnect,
      outerLayout :: Layout
      }
  | Fork { key :: Int,
           outerLayout :: Layout,
           lengthXY :: Double,
           rightC :: RightConnect
         }
  | Hist { key :: Int,
           history :: HistoryType,
           lengthXY :: Double,
           rightC :: RightConnect,
           outerLayout :: Layout
         }
  | Leaf { key :: Int,
           strings :: String,
           operation :: String,
           lengthXY :: Double,
           rightC :: RightConnect,
           outerLayout :: Layout
         }
  | StartS { key :: Int,
             lengthXY :: Double,
             rightC :: RightConnect,
             outerLayout :: Layout
           }
  | CrossStateDummy {
      key :: Int,
      lengthXY :: Double
      }
  | Dummy { key :: Int,
            outerLayout :: Layout,
            lengthXY :: Double
          }
  | Transition { key :: Int,
                 transitionName :: String,
                 lengthXY :: Double,
                 rightC :: RightConnect,
                 outerLayout :: Layout
               }
  deriving (Read, Show)

data ConnectWithType = ConnectWithType { connecting :: Connection Int,
                                         connectType :: ConnectionType
                                       }
  deriving (Read, Show)

data Connection label =  Connection {
  pointFrom :: [label],
  pointTo :: [label],
  transition :: String
  }
  deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)

-- ForwardH = forwardArrowWithHead | SelfCL = selfConnectLeft
data ConnectionType = ForwardH | ForwardWH | BackwardH | BackwardWH | SelfCL |
  SelfCR
  deriving (Eq, Read, Show)

data HistoryType = Shallow | Deep
  deriving (Eq, Ord, Read, Show)

{-# DEPRECATED unUML' "Use unUML instead" #-}
newtype UMLStateDiagram n l = UMLStateDiagram {unUML' :: StateDiagram n l [Connection l]}
  deriving Generic

unUML ::
  (n ->
   [StateDiagram n a [Connection a]] ->
   [Connection a] ->
   [a] ->
   b)
  -> UMLStateDiagram n a -> b
unUML c (UMLStateDiagram StateDiagram{substate, name, connection, startState}) = c name substate connection startState
unUML _ _ = error "should never happen"

umlStateDiagram :: StateDiagram n a [Connection a] -> UMLStateDiagram n a
umlStateDiagram s@StateDiagram{} = UMLStateDiagram s
umlStateDiagram _ = error "should never happen"

rename :: (n -> m) -> UMLStateDiagram n a -> UMLStateDiagram m a
rename f = unUML $
  \name substate connection startState ->
    umlStateDiagram $
    StateDiagram { name = f name,
                   substate = map recurse substate,
                   label = undefined,
                   ..
                 }
  where
    -- recurse :: StateDiagram n a -> StateDiagram m a
    recurse StateDiagram{..} = StateDiagram{name = f name, substate = map recurse substate, ..}
    recurse CombineDiagram{..} = CombineDiagram{substate = map recurse substate, ..}
    recurse EndState{..} = EndState{..}
    recurse Joint{..} = Joint{..}
    recurse History{..} = History{..}
    recurse InnerMostState{..} = InnerMostState{name = f name, ..}

data StateDiagram n l a = StateDiagram { substate :: [StateDiagram n l a],
                                          label :: l,
                                          name :: n,
                                          connection :: a,
                                          startState :: [l]
                                        }
                     | CombineDiagram { substate :: [StateDiagram n l a],
                                        label :: l
                                      }
                     | EndState {
                         label :: l
                         }
                     | Joint { label :: l
                             }
                     | History { label :: l,
                                 historyType :: HistoryType
                               }
                     | InnerMostState { label :: l,
                                        name :: n,
                                        operations :: String
                                      }
  deriving (Eq, Foldable, Functor, Read, Show, Traversable)

data Layout = Vertical | Horizontal | Unspecified
  deriving (Eq, Read, Show)

data RightConnect = WithArrowhead | WithoutArrowhead | NoConnection
  deriving (Eq, Read, Show)

$(deriveBifunctor ''StateDiagram)
$(deriveBifoldable ''StateDiagram)
$(deriveBitraversable ''StateDiagram)

globalise :: UMLStateDiagram n a -> UMLStateDiagram n a
globalise (UMLStateDiagram s@StateDiagram{ substate }) = UMLStateDiagram $
                                       s { connection = hoistOutwards s
                                         , substate = map (fmap (const [])) substate }
globalise _ = error "should never happen"

hoistOutwards :: StateDiagram n a [Connection a] -> [Connection a]
hoistOutwards StateDiagram{ substate, connection }
  = connection ++ concatMap (\d -> prependL (label d) (hoistOutwards d)) substate
hoistOutwards CombineDiagram{ substate }
  = concatMap (\d -> prependL (label d) (hoistOutwards d)) substate
hoistOutwards _ = []

prependL :: a -> [Connection a] -> [Connection a]
prependL l =
    map (\c@Connection{ pointFrom, pointTo }
          -> c { pointFrom = l : pointFrom, pointTo = l : pointTo })

localiseConnection :: Eq a => Connection a -> ([a], Connection a)
localiseConnection c = commonPrefix (pointFrom c) (pointTo c)
  where
    commonPrefix []  _  = error "connection has no source"
    commonPrefix _   [] = error "connection has no target"
    commonPrefix [x] ys = ([], c { pointFrom = [x], pointTo = ys })
    commonPrefix xs [y] = ([], c { pointFrom = xs, pointTo = [y] })
    commonPrefix (x:xs) (y:ys)
      | x == y
      = let (cps, c') = commonPrefix xs ys
        in (x:cps, c')
      | otherwise
      = ([], c { pointFrom = x:xs, pointTo = y:ys })

localise :: Eq a => StateDiagram n a [Connection a] -> StateDiagram n a [Connection a]
localise = localiseStateDiagram []

localiseStateDiagram :: Eq a => [([a], Connection a)] -> StateDiagram n a [Connection a] -> StateDiagram n a [Connection a]
localiseStateDiagram cs s = case s of
  StateDiagram {}   -> s {
    connection = map snd local,
    substate   = subs
    }
  CombineDiagram {} -> s {
    substate   = subs
    }
  _                 -> s
  where
    matching = [ (cps, c) | (cp:cps, c) <- cs, label s == cp]
    (local, global) = partition (null . fst)
      $ matching ++ map localiseConnection connections
    subs     = map (localiseStateDiagram global) $ substate s
    connections
      | StateDiagram {} <- s = connection s
      | otherwise            = []
