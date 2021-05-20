{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Datatype where

data Wrapper =
  OrDecom { layered :: [[Wrapper]],
            key :: Int,
            strings :: String,
            connections :: [ConnectWithType],
            layout :: Layout,
            maxLabel :: Int,
            lengthXY :: Double,
            rightC :: RightConnect,
            outerLayout :: Layout
          }
  | AndDecom { component :: [Wrapper],
               key :: Int,
               layout :: Layout,
               lengthXY :: Double,
               rightC :: RightConnect,
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
  | CrossStateDummy { key :: Int }
  | Dummy { key :: Int,
            outerLayout :: Layout,
            lengthXY :: Double
          }
  | Transition { key :: Int,
                 transitionName :: String,
                 lengthXY :: Double,
                 rightC :: RightConnect,
                 outerLayout :: Layout
               } deriving Show

data ConnectWithType = ConnectWithType { connecting :: Connection,
                                         connectType :: ConnectionType
                                       } deriving Show

data Connection =  Connection {
  pointFrom :: [Int],
  pointTo :: [Int],
  transition :: String
  } deriving Show

-- ForwardH = forwardArrowWithHead | SelfCL = selfConnectLeft
data ConnectionType = ForwardH | ForwardWH | BackwardH | BackwardWH | SelfCL |
  SelfCR deriving (Show, Eq)

data HistoryType = Shallow | Deep
  deriving (Show, Eq)

data UMLStateDiagram = StateDiagram { substate :: [UMLStateDiagram],
                                      label :: Int,
                                      name :: String,
                                      connection :: [Connection],
                                      startState :: [Int]
                                    }
                     | CombineDiagram { substate :: [UMLStateDiagram],
                                        label :: Int
                                      }
                     | Joint { label :: Int
                             }
                     | History { label :: Int,
                                 historyType :: HistoryType
                               }
                     | InnerMostState { label :: Int,
                                        name :: String,
                                        operations :: String
                                      } deriving Show

data Layout = Vertical | Horizontal | Unspecified deriving (Show, Eq)

data RightConnect = WithArrowhead | WithoutArrowhead | NoConnection deriving (Show, Eq)
