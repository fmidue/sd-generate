{-# OPTIONS_GHC -Wno-deprecations   #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module FlattenSpec (
  spec
) where

import Test.Hspec
import Datatype (StateDiagram(..)
                ,Connection (..)
                ,UMLStateDiagram (unUML')
                ,HistoryType(..)
                ,umlStateDiagram
                ,unUML
                ,globalise
                ,rename
                )
import Example (flatCase1
               ,flatCase3
               ,flatCase7
               ,task26a
               ,slide267b
               ,slide281
               ,task88
               ,test4
               ,noHistoryTest4
               ,implicitExitTask85
               ,noHistorySlide281
               ,selfTransitionCase1)
import Flatten (flatten
               ,flatten')
import Data.List (sortBy)
import Data.String (String)

connectionsOrderedByName :: UMLStateDiagram [String] Int -> UMLStateDiagram [String] Int
connectionsOrderedByName
  = umlStateDiagram
    . unUML (\rootName rootSubstates rootConnections rootInitial ->
        StateDiagram { name = rootName
                     , substates = rootSubstates
                     , connections = sortBy (\(Connection _ _ x) (Connection _ _ y) -> compare x y) rootConnections
                     , startState = rootInitial
                     , label = error "NOT RELEVANT" } )
    . globalise

flatCase1Res :: UMLStateDiagram [String] Int
flatCase1Res
  = umlStateDiagram $ StateDiagram {substates =
    [InnerMostState {label = 1, name = ["P","G"], operations = ""}
    ,InnerMostState {label = 2, name = ["P","H"], operations = ""}
    ,InnerMostState {label = 3, name = ["A"], operations = ""}
    ,InnerMostState {label = 4, name = ["B"], operations = ""}
    ,InnerMostState {label = 5, name = ["C"], operations = ""}]
    , label = 0, name = [""]
    , connections =
    [Connection {pointFrom = [3], pointTo = [2], transition = "a"}
    ,Connection {pointFrom = [1], pointTo = [4], transition = "c"}
    ,Connection {pointFrom = [4], pointTo = [5], transition = "d"}
    ,Connection {pointFrom = [5], pointTo = [1], transition = "e"}
    ,Connection {pointFrom = [2], pointTo = [1], transition = "b"}]
    , startState = [2]}

flatCase3Res1Step :: UMLStateDiagram [String] Int
flatCase3Res1Step
  = umlStateDiagram $ StateDiagram { substates = [
    InnerMostState { label = 1, name = ["P1", "G"], operations = "" }
    ,InnerMostState { label = 2, name = ["P1", "H"], operations = "" }
    ,InnerMostState { label = 3, name = ["A"], operations = "" }
    ,InnerMostState { label = 4, name = ["B"], operations = "" }
    ,InnerMostState { label = 5, name = ["C"], operations = "" }
    ,StateDiagram { label = 6
                  , name = ["P2"]
                  , startState = [2]
                  , connections = []
                  , substates = [InnerMostState { label = 1, name = ["I"], operations = ""}
                                ,InnerMostState { label = 2, name = ["J"], operations = ""}]
    }]
    ,startState = [2]
    ,label = error "not relevant"
    ,name = [""]
    ,connections =
    [Connection {pointFrom = [3], pointTo = [2], transition = "a"}
    ,Connection {pointFrom = [1], pointTo = [4], transition = "c"}
    ,Connection {pointFrom = [4], pointTo = [5], transition = "d"}
    ,Connection {pointFrom = [5], pointTo = [1], transition = "e"}
    ,Connection {pointFrom = [2], pointTo = [1], transition = "b"}
    ,Connection {pointFrom = [6,1], pointTo = [6,2], transition = "i"}]}

flatCase7Res1Step :: UMLStateDiagram [String] Int
flatCase7Res1Step
  = let
    isA = InnerMostState 3 ["A"] ""
    isB = InnerMostState 4 ["B"] ""
    isC = InnerMostState 5 ["C"] ""
    isPG = InnerMostState 1 ["P","G"] ""
    isPH = InnerMostState 2 ["P","H"] ""
    isE = InnerMostState 1 ["E"] ""
    isF = InnerMostState 2 ["F"] ""
    sd1 = StateDiagram [isE,isF] 6 ["D"]
          []
          [1]
    in
    umlStateDiagram $ StateDiagram [isPG,isPH,isA,isB,isC,sd1] 0 [""]
      [ Connection [3] [2] "a"
      , Connection [2] [1] "b"
      , Connection [1] [4] "c"
      , Connection [4] [5] "d"
      , Connection [5] [1] "e"
      , Connection [5] [6] "f"
      , Connection [6,2] [3] "g"
      , Connection [6,1] [6,2] "h" ]
    [2]

task26aStep1Res :: UMLStateDiagram [String] Int
task26aStep1Res
  = umlStateDiagram $
    StateDiagram {
      substates = [ InnerMostState {label = 1, name = ["","G"], operations = ""}
                  , InnerMostState {label = 2, name = ["","H"], operations = ""}
                  , CombineDiagram {
                      substates = [ StateDiagram {
                                      substates = [ InnerMostState {label = 1, name = ["A"], operations = ""}
                                                  , InnerMostState {label = 2, name = ["B"], operations = ""}
                                                  , InnerMostState {label = 3, name = ["C"], operations = ""}]
                                    , label = 1
                                    , name = [""]
                                    , connections = []
                                    , startState = [1] }
                                  , StateDiagram {
                                      substates = [ InnerMostState {label = 1, name = ["D"], operations = ""}
                                                  , InnerMostState {label = 2, name = ["E"], operations = ""}]
                                  , label = 2
                                  , name = [""]
                                  , connections = []
                                  , startState = [1]}]
                    , label = 3}
                  , ForkOrJoin {label = 4}
                  , InnerMostState {label = 5, name = ["F"], operations = ""}]
    , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
    , name = [""]
    , connections = [Connection {pointFrom = [3,1,3], pointTo = [4], transition = "e"}
                    ,Connection {pointFrom = [3,2,2], pointTo = [4], transition = "e"}
                    ,Connection {pointFrom = [4], pointTo = [5], transition = ""}
                    ,Connection {pointFrom = [5], pointTo = [2], transition = "c"}
                    ,Connection {pointFrom = [5], pointTo = [2], transition = "d"}
                    ,Connection {pointFrom = [2], pointTo = [3,2,2], transition = "a"}
                    ,Connection {pointFrom = [1], pointTo = [3], transition = "a"}
                    ,Connection {pointFrom = [3,1,1], pointTo = [3,1,2], transition = "a"}
                    ,Connection {pointFrom = [3,1,2], pointTo = [3,1,3], transition = "b"}
                    ,Connection {pointFrom = [3,1,3], pointTo = [3,1,1], transition = "a"}
                    ,Connection {pointFrom = [3,2,1], pointTo = [3,2,2], transition = "b"}
                    ,Connection {pointFrom = [3,2,2], pointTo = [3,2,1], transition = "c"}
                    ,Connection {pointFrom = [2], pointTo = [1], transition = "d"}]
   , startState = [3,1,2]}

{- checked manually -}
slide267bRes1Step :: UMLStateDiagram [String] Int
slide267bRes1Step
  = umlStateDiagram $
    StateDiagram {
      substates = [ InnerMostState {label = 1, name = ["A","B"], operations = ""}
                  , InnerMostState {label = 2, name = ["A","C"], operations = ""}
                  , InnerMostState {label = 3, name = ["A","D"], operations = ""}
                  , InnerMostState {label = 4, name = ["E"], operations = ""} ]
    , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
    , name = [""]
    , connections = [ Connection {pointFrom = [1], pointTo = [4], transition = "a"}
                    , Connection {pointFrom = [2], pointTo = [4], transition = "a"}
                    , Connection {pointFrom = [3], pointTo = [4], transition = "a"}
                    , Connection {pointFrom = [1], pointTo = [2], transition = "b"}
                    , Connection {pointFrom = [2], pointTo = [3], transition = "c"}
                    , Connection {pointFrom = [3], pointTo = [2], transition = "b"}
                    , Connection {pointFrom = [2], pointTo = [1], transition = "b"} ]
    , startState = [1] }

{- taken from console output; to be verified; but expected to be ok -}
task88Res1Step :: UMLStateDiagram [String] Int
task88Res1Step
  = umlStateDiagram $
    StateDiagram {
      substates = [ InnerMostState {label = 1, name = ["","F"], operations = ""}
                  , InnerMostState {label = 2, name = ["","G"], operations = ""}
                  , CombineDiagram {
                      substates = [ StateDiagram {
                                      substates = [ InnerMostState {label = 1, name = ["A"], operations = ""}
                                                  , InnerMostState {label = 2, name = ["B"], operations = ""}]
                                    , label = 1
                                    , name = [""]
                                    , connections = []
                                    , startState = [1]}
                  , StateDiagram {
                      substates = [ InnerMostState {label = 1, name = ["C"], operations = ""}
                                  , InnerMostState {label = 2, name = ["D"], operations = ""}]
                    , label = 2
                    , name = [""]
                    , connections = []
                    , startState = [1]}]
                    , label = 3}
                  , InnerMostState {label = 4, name = ["E"], operations = ""}]
    , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
    , name = [""]
    , connections = [ Connection {pointFrom = [3], pointTo = [1], transition = "k"}
                    , Connection {pointFrom = [4], pointTo = [1], transition = "k"}
                    , Connection {pointFrom = [2], pointTo = [4], transition = "h"}
                    , Connection {pointFrom = [4], pointTo = [3,2,2], transition = "h"}
                    , Connection {pointFrom = [3,1,1], pointTo = [3,1,2], transition = "a"}
                    , Connection {pointFrom = [3,1,2], pointTo = [3,1,1], transition = "a"}
                    , Connection {pointFrom = [3,2,1], pointTo = [3,2,2], transition = "b"}
                    , Connection {pointFrom = [3,2,2], pointTo = [3,2,1], transition = "b"}
                    , Connection {pointFrom = [1], pointTo = [2], transition = "a"}
                    , Connection {pointFrom = [2], pointTo = [1], transition = "a"}]
    , startState = [1] }


-- this result is likely wrong
test4Res1Step :: UMLStateDiagram [String] Int
test4Res1Step
  = umlStateDiagram $
    StateDiagram {
      substates = [ StateDiagram {
                      substates = [ ForkOrJoin { label = 1 }
                                  , CombineDiagram {
                                      substates = [ StateDiagram {
                                                      substates = [ InnerMostState {label = 1, name = ["1"], operations = ""}
                                                                  , InnerMostState {label = 2, name = ["2"], operations = ""} ]
                                                    , label = 1
                                                    , name = ["C"]
                                                    , connections = []
                                                    , startState = [1]}
                                                  , StateDiagram {
                                                      substates = [ InnerMostState {label = 1, name = ["3"], operations = ""}
                                                                  , InnerMostState {label = 2, name = ["4"], operations = ""}
                                                                  , InnerMostState {label = 3, name = ["5"], operations = ""}]
                                                    , label = 2
                                                    , name = [""]
                                                    , connections = []
                                                    , startState = [1]}]
                                    , label = 2}
                                  , ForkOrJoin {label = 3}
                                  , History {label = 4, historyType = Shallow}
                                  , StateDiagram {
                                      substates = [ InnerMostState {label = 1, name = ["9"], operations = ""}
                                                  , InnerMostState {label = 2, name = ["10"], operations = ""}]
                                    , label = 5
                                    , name = ["D"]
                                    , connections = []
                                    , startState = [1]}]
                    , label = 1
                    , name = ["A","B"]
                    , connections = []
                    , startState = []}
                    , StateDiagram {
                        substates = [ InnerMostState {label = 1, name = ["6"], operations = ""}
                                    , InnerMostState {label = 2, name = ["7"], operations = ""}
                                    , InnerMostState {label = 3, name = ["8"], operations = ""}]
                      , label = 2
                      , name = ["A","E"]
                      , connections = []
                      , startState = [3]}]
    , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
    , name = [""]
    , connections = [ Connection {pointFrom = [1,2,1,1], pointTo = [2], transition = "i"}
                    , Connection {pointFrom = [1,3], pointTo = [2], transition = ""}
                    , Connection {pointFrom = [2,2], pointTo = [1,2,2,3], transition = "e"}
                    , Connection {pointFrom = [1,5,2], pointTo = [2,2], transition = "h"}
                    , Connection {pointFrom = [2,1], pointTo = [1,4], transition = "f"}
                    , Connection {pointFrom = [1,4], pointTo = [1,5], transition = ""}
                    , Connection {pointFrom = [1,1], pointTo = [1,2,1,1], transition = ""}
                    , Connection {pointFrom = [1,1], pointTo = [1,2,2,3], transition = ""}
                    , Connection {pointFrom = [1,2,1,2], pointTo = [1,3], transition = "c"}
                    , Connection {pointFrom = [1,2,2,2], pointTo = [1,3], transition = "c"}
                    , Connection {pointFrom = [1,2,1,1], pointTo = [1,2,1,2], transition = "a"}
                    , Connection {pointFrom = [1,2,1,2], pointTo = [1,2,1,1], transition = "a"}
                    , Connection {pointFrom = [1,2,2,1], pointTo = [1,2,2,2], transition = "b"}
                    , Connection {pointFrom = [1,2,2,2], pointTo = [1,2,2,3], transition = "b"}
                    , Connection {pointFrom = [1,2,2,3], pointTo = [1,2,2,1], transition = "b"}
                    , Connection {pointFrom = [1,5,1], pointTo = [1,5,2], transition = "g"}
                    , Connection {pointFrom = [1,5,2], pointTo = [1,5,1], transition = "g"}
                    , Connection {pointFrom = [2,1], pointTo = [2,3], transition = "d"}
                    , Connection {pointFrom = [2,3], pointTo = [2,2], transition = "d"}
                    , Connection {pointFrom = [2,2], pointTo = [2,1], transition = "d"}]
    , startState = [1,1]}

noHistoryTest4Res1Step :: UMLStateDiagram [String] Int
noHistoryTest4Res1Step
  = umlStateDiagram $
    StateDiagram {
      substates = [StateDiagram {
                     substates = [ForkOrJoin {label = 1}
                                 ,CombineDiagram {
                                    substates = [StateDiagram {
                                                  substates = [InnerMostState {label = 1, name = ["1"], operations = ""}
                                                              ,InnerMostState {label = 2, name = ["2"], operations = ""}]
                                                , label = 1
                                                , name = ["C"]
                                                , connections = []
                                                , startState = [1]}
                                               ,StateDiagram {
                                                  substates = [InnerMostState {label = 1, name = ["3"], operations = ""}
                                                              ,InnerMostState {label = 2, name = ["4"], operations = ""}
                                                              ,InnerMostState {label = 3, name = ["5"], operations = ""}]
                                                , label = 2
                                                , name = [""]
                                                , connections = []
                                                , startState = [1]}]
                                                , label = 2}
                                               ,ForkOrJoin {label = 3}
                                               ,StateDiagram {
                                                  substates = [InnerMostState {label = 1, name = ["9"], operations = ""}
                                                              ,InnerMostState {label = 2, name = ["10"], operations = ""}]
                                                , label = 5
                                                , name = ["D"]
                                                , connections = []
                                                , startState = [1]}]
                                                , label = 1
                                                , name = ["A","B"]
                                                , connections = []
                                                , startState = []}
                                               ,StateDiagram {
                                                  substates = [InnerMostState {label = 1, name = ["6"], operations = ""}
                                                              ,InnerMostState {label = 2, name = ["7"], operations = ""}
                                                              ,InnerMostState {label = 3, name = ["8"], operations = ""}]
                                                , label = 2
                                                , name = ["A","E"]
                                                , connections = []
                                                , startState = [3]}]
                                                , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
                                                , name = [""]
                                                , connections = [Connection {pointFrom = [1,2,1,1], pointTo = [2], transition = "i"}
                                                                ,Connection {pointFrom = [1,3], pointTo = [2], transition = ""}
                                                                ,Connection {pointFrom = [2,2], pointTo = [1,2,2,3], transition = "e"}
                                                                ,Connection {pointFrom = [1,5,2], pointTo = [2,2], transition = "h"}
                                                                ,Connection {pointFrom = [1,1], pointTo = [1,2,1,1], transition = ""}
                                                                ,Connection {pointFrom = [1,1], pointTo = [1,2,2,3], transition = ""}
                                                                ,Connection {pointFrom = [1,2,1,2], pointTo = [1,3], transition = "c"}
                                                                ,Connection {pointFrom = [1,2,2,2], pointTo = [1,3], transition = "c"}
                                                                ,Connection {pointFrom = [1,2,1,2], pointTo = [1,5], transition = "enter"}
                                                                ,Connection {pointFrom = [1,5,1], pointTo = [1,2,2,3], transition = "exit"}
                                                                ,Connection {pointFrom = [1,2,1,1], pointTo = [1,2,1,2], transition = "a"}
                                                                ,Connection {pointFrom = [1,2,1,2], pointTo = [1,2,1,1], transition = "a"}
                                                                ,Connection {pointFrom = [1,2,2,1], pointTo = [1,2,2,2], transition = "b"}
                                                                ,Connection {pointFrom = [1,2,2,2], pointTo = [1,2,2,3], transition = "b"}
                                                                ,Connection {pointFrom = [1,2,2,3], pointTo = [1,2,2,1], transition = "b"}
                                                                ,Connection {pointFrom = [1,5,1], pointTo = [1,5,2], transition = "g"}
                                                                ,Connection {pointFrom = [1,5,2], pointTo = [1,5,1], transition = "g"}
                                                                ,Connection {pointFrom = [2,1], pointTo = [2,3], transition = "d"}
                                                                ,Connection {pointFrom = [2,3], pointTo = [2,2], transition = "d"}
                                                                ,Connection {pointFrom = [2,2], pointTo = [2,1], transition = "d"}]
                                                , startState = [1,1]}

noHistoryTest4Res2Step :: UMLStateDiagram [String] Int
noHistoryTest4Res2Step
  = umlStateDiagram $
    StateDiagram {
      substates = [ForkOrJoin {label = 1}
                  ,CombineDiagram {
                    substates = [StateDiagram {
                                   substates = [InnerMostState {label = 1, name = ["1"], operations = ""}
                                               ,InnerMostState {label = 2, name = ["2"], operations = ""}]
                                 , label = 1
                                 , name = ["C"]
                                 , connections = []
                                 , startState = [1]}
                                ,StateDiagram {
                                   substates = [InnerMostState {label = 1, name = ["3"], operations = ""}
                                               ,InnerMostState {label = 2, name = ["4"], operations = ""}
                                               ,InnerMostState {label = 3, name = ["5"], operations = ""}]
                                 , label = 2
                                 , name = [""]
                                 , connections = []
                                 , startState = [1]}]
                                 , label = 2}
                                ,ForkOrJoin {label = 3}
                                ,StateDiagram {
                                   substates = [InnerMostState {label = 1, name = ["9"], operations = ""}
                                               ,InnerMostState {label = 2, name = ["10"], operations = ""}]
                                 , label = 4
                                 , name = ["A","B","D"]
                                 , connections = []
                                 , startState = [1]}
                                ,StateDiagram {
                                   substates = [InnerMostState {label = 1, name = ["6"], operations = ""}
                                               ,InnerMostState {label = 2, name = ["7"], operations = ""}
                                               ,InnerMostState {label = 3, name = ["8"], operations = ""}]
                                 , label = 5
                                 , name = ["A","E"]
                                 , connections = []
                                 , startState = [3]}]
                                 , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
                                 , name = [""]
                                 , connections = [Connection {pointFrom = [2,1,1], pointTo = [5], transition = "i"}
                                                 ,Connection {pointFrom = [3], pointTo = [5], transition = ""}
                                                 ,Connection {pointFrom = [5,2], pointTo = [2,2,3], transition = "e"}
                                                 ,Connection {pointFrom = [4,2], pointTo = [5,2], transition = "h"}
                                                 ,Connection {pointFrom = [1], pointTo = [2,1,1], transition = ""}
                                                 ,Connection {pointFrom = [1], pointTo = [2,2,3], transition = ""}
                                                 ,Connection {pointFrom = [2,1,2], pointTo = [3], transition = "c"}
                                                 ,Connection {pointFrom = [2,2,2], pointTo = [3], transition = "c"}
                                                 ,Connection {pointFrom = [2,1,2], pointTo = [4], transition = "enter"}
                                                 ,Connection {pointFrom = [4,1], pointTo = [2,2,3], transition = "exit"}
                                                 ,Connection {pointFrom = [2,1,1], pointTo = [2,1,2], transition = "a"}
                                                 ,Connection {pointFrom = [2,1,2], pointTo = [2,1,1], transition = "a"}
                                                 ,Connection {pointFrom = [2,2,1], pointTo = [2,2,2], transition = "b"}
                                                 ,Connection {pointFrom = [2,2,2], pointTo = [2,2,3], transition = "b"}
                                                 ,Connection {pointFrom = [2,2,3], pointTo = [2,2,1], transition = "b"}
                                                 ,Connection {pointFrom = [4,1], pointTo = [4,2], transition = "g"}
                                                 ,Connection {pointFrom = [4,2], pointTo = [4,1], transition = "g"}
                                                 ,Connection {pointFrom = [5,1], pointTo = [5,3], transition = "d"}
                                                 ,Connection {pointFrom = [5,3], pointTo = [5,2], transition = "d"}
                                                 ,Connection {pointFrom = [5,2], pointTo = [5,1], transition = "d"}]
                                 , startState = [1]}

noHistoryTest4Res3Step :: UMLStateDiagram [String] Int
noHistoryTest4Res3Step
  = umlStateDiagram $
    StateDiagram {
      substates = [InnerMostState {label = 1, name = ["A","B","D","9"], operations = ""}
                  ,InnerMostState {label = 2, name = ["A","B","D","10"], operations = ""}
                  ,ForkOrJoin {label = 3}
                  ,CombineDiagram {
                     substates = [StateDiagram {
                                    substates = [InnerMostState {label = 1, name = ["1"], operations = ""}
                                                ,InnerMostState {label = 2, name = ["2"], operations = ""}]
                                  , label = 1
                                  , name = ["C"]
                                  , connections = []
                                  , startState = [1]}
                                 ,StateDiagram {
                                    substates = [InnerMostState {label = 1, name = ["3"], operations = ""}
                                                ,InnerMostState {label = 2, name = ["4"], operations = ""}
                                                ,InnerMostState {label = 3, name = ["5"], operations = ""}]
                                  , label = 2
                                  , name = [""]
                                  , connections = []
                                  , startState = [1]}]
                                  , label = 4}
                  ,ForkOrJoin {label = 5}
                  ,StateDiagram {
                     substates = [InnerMostState {label = 1, name = ["6"], operations = ""}
                                 ,InnerMostState {label = 2, name = ["7"], operations = ""}
                                 ,InnerMostState {label = 3, name = ["8"], operations = ""}]
                   , label = 6, name = ["A","E"], connections = [], startState = [3]}]
                   , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
                   , name = [""]
                   , connections = [Connection {pointFrom = [4,1,1], pointTo = [6], transition = "i"}
                                   ,Connection {pointFrom = [5], pointTo = [6], transition = ""}
                                   ,Connection {pointFrom = [6,2], pointTo = [4,2,3], transition = "e"}
                                   ,Connection {pointFrom = [2], pointTo = [6,2], transition = "h"}
                                   ,Connection {pointFrom = [3], pointTo = [4,1,1], transition = ""}
                                   ,Connection {pointFrom = [3], pointTo = [4,2,3], transition = ""}
                                   ,Connection {pointFrom = [4,1,2], pointTo = [5], transition = "c"}
                                   ,Connection {pointFrom = [4,2,2], pointTo = [5], transition = "c"}
                                   ,Connection {pointFrom = [4,1,2], pointTo = [1], transition = "enter"}
                                   ,Connection {pointFrom = [1], pointTo = [4,2,3], transition = "exit"}
                                   ,Connection {pointFrom = [4,1,1], pointTo = [4,1,2], transition = "a"}
                                   ,Connection {pointFrom = [4,1,2], pointTo = [4,1,1], transition = "a"}
                                   ,Connection {pointFrom = [4,2,1], pointTo = [4,2,2], transition = "b"}
                                   ,Connection {pointFrom = [4,2,2], pointTo = [4,2,3], transition = "b"}
                                   ,Connection {pointFrom = [4,2,3], pointTo = [4,2,1], transition = "b"}
                                   ,Connection {pointFrom = [1], pointTo = [2], transition = "g"}
                                   ,Connection {pointFrom = [2], pointTo = [1], transition = "g"}
                                   ,Connection {pointFrom = [6,1], pointTo = [6,3], transition = "d"}
                                   ,Connection {pointFrom = [6,3], pointTo = [6,2], transition = "d"}
                                   ,Connection {pointFrom = [6,2], pointTo = [6,1], transition = "d"}]
                   , startState = [3]}

noHistoryTest4Res4Step :: UMLStateDiagram [String] Int
noHistoryTest4Res4Step
  = umlStateDiagram $
    StateDiagram {
      substates = [InnerMostState {label = 1, name = ["A","E","6"], operations = ""}
                  ,InnerMostState {label = 2, name = ["A","E","7"], operations = ""}
                  ,InnerMostState {label = 3, name = ["A","E","8"], operations = ""}
                  ,InnerMostState {label = 4, name = ["A","B","D","9"], operations = ""}
                  ,InnerMostState {label = 5, name = ["A","B","D","10"], operations = ""}
                  ,ForkOrJoin {label = 6}
                  ,CombineDiagram {
                     substates = [StateDiagram {
                                    substates = [InnerMostState {label = 1, name = ["1"], operations = ""}
                                                ,InnerMostState {label = 2, name = ["2"], operations = ""}]
                                  , label = 1
                                  , name = ["C"]
                                  , connections = []
                                  , startState = [1]}
                                 ,StateDiagram {
                                    substates = [InnerMostState {label = 1, name = ["3"], operations = ""}
                                                ,InnerMostState {label = 2, name = ["4"], operations = ""}
                                                ,InnerMostState {label = 3, name = ["5"], operations = ""}]
                                  , label = 2
                                  , name = [""]
                                  , connections = []
                                  , startState = [1]}]
                                  , label = 7}
                                 ,ForkOrJoin {label = 8}]
                   , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
                   , name = [""]
                   , connections = [Connection {pointFrom = [7,1,1], pointTo = [3], transition = "i"}
                                   ,Connection {pointFrom = [8], pointTo = [3], transition = ""}
                                   ,Connection {pointFrom = [2], pointTo = [7,2,3], transition = "e"}
                                   ,Connection {pointFrom = [5], pointTo = [2], transition = "h"}
                                   ,Connection {pointFrom = [6], pointTo = [7,1,1], transition = ""}
                                   ,Connection {pointFrom = [6], pointTo = [7,2,3], transition = ""}
                                   ,Connection {pointFrom = [7,1,2], pointTo = [8], transition = "c"}
                                   ,Connection {pointFrom = [7,2,2], pointTo = [8], transition = "c"}
                                   ,Connection {pointFrom = [7,1,2], pointTo = [4], transition = "enter"}
                                   ,Connection {pointFrom = [4], pointTo = [7,2,3], transition = "exit"}
                                   ,Connection {pointFrom = [7,1,1], pointTo = [7,1,2], transition = "a"}
                                   ,Connection {pointFrom = [7,1,2], pointTo = [7,1,1], transition = "a"}
                                   ,Connection {pointFrom = [7,2,1], pointTo = [7,2,2], transition = "b"}
                                   ,Connection {pointFrom = [7,2,2], pointTo = [7,2,3], transition = "b"}
                                   ,Connection {pointFrom = [7,2,3], pointTo = [7,2,1], transition = "b"}
                                   ,Connection {pointFrom = [4], pointTo = [5], transition = "g"}
                                   ,Connection {pointFrom = [5], pointTo = [4], transition = "g"}
                                   ,Connection {pointFrom = [1], pointTo = [3], transition = "d"}
                                   ,Connection {pointFrom = [3], pointTo = [2], transition = "d"}
                                   ,Connection {pointFrom = [2], pointTo = [1], transition = "d"}]
                    , startState = [6]}

noHistorySlide281Res1Step :: UMLStateDiagram [String] Int
noHistorySlide281Res1Step
  = umlStateDiagram $
    StateDiagram { substates = [ InnerMostState {label = 1, name = ["","C"], operations = ""}
                               , InnerMostState {label = 2, name = ["","D"], operations = ""}
                               , InnerMostState {label = 3, name = ["A"], operations = ""}
                               , InnerMostState {label = 4, name = ["B"], operations = ""}
                               , InnerMostState {label = 5, name = ["X"], operations = ""}]
                 , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
                 , name = [""]
                 , connections = [ Connection {pointFrom = [3], pointTo = [4], transition = "a"}
                                 , Connection {pointFrom = [4], pointTo = [3], transition = "f"}
                                 , Connection {pointFrom = [1], pointTo = [3], transition = "e"}
                                 , Connection {pointFrom = [2], pointTo = [3], transition = "e"}
                                 , Connection {pointFrom = [5], pointTo = [1], transition = "x"}
                                 , Connection {pointFrom = [1], pointTo = [2], transition = "c"}
                                 , Connection {pointFrom = [2], pointTo = [1], transition = "d"}]
                 , startState = [5]}

implicitExitTask85Res1Step :: UMLStateDiagram [String] Int
implicitExitTask85Res1Step
  = umlStateDiagram $
    StateDiagram { substates = [ CombineDiagram { substates = [ StateDiagram { substates = [ InnerMostState {label = 1, name = ["1"], operations = ""}
                                                                                           , InnerMostState {label = 2, name = ["2"], operations = ""}]
                                                                             , label = 1, name = ["C"]
                                                                             , connections = []
                                                                             , startState = [1]}
                                                              , StateDiagram { substates = [ InnerMostState {label = 1, name = ["3"], operations = ""}
                                                                                           , InnerMostState {label = 2, name = ["4"], operations = ""}
                                                                                           , InnerMostState {label = 3, name = ["5"], operations = ""}]
                                                                             , label = 2, name = [""]
                                                                             , connections = []
                                                                             , startState = [1]}]
                                                                             , label = 1}
                                                              , StateDiagram { substates = [ InnerMostState {label = 1, name = ["9"], operations = ""}
                                                                                           , InnerMostState {label = 2, name = ["10"], operations = ""}]
                                                                             , label = 2
                                                                             , name = ["A","B","D"]
                                                                             , connections = []
                                                                             , startState = [1]}
                                                              , ForkOrJoin {label = 3}
                                                              , StateDiagram { substates = [ InnerMostState {label = 1, name = ["6"], operations = ""}
                                                                                           , InnerMostState {label = 2, name = ["7"], operations = ""}
                                                                                           , InnerMostState {label = 3, name = ["8"], operations = ""}]
                                                                             , label = 4
                                                                             , name = ["A","E"]
                                                                             , connections = []
                                                                             , startState = [3]}
                                                              , ForkOrJoin {label = 5}]
                 , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
                 , name = [""]
                 , connections = [ Connection {pointFrom = [5], pointTo = [1,1,1], transition = ""}
                                 , Connection {pointFrom = [5], pointTo = [1,2,1], transition = ""}
                                 , Connection {pointFrom = [1,1,1], pointTo = [4], transition = "i"}
                                 , Connection {pointFrom = [3], pointTo = [4], transition = ""}
                                 , Connection {pointFrom = [2,2], pointTo = [4,2], transition = "h"}
                                 , Connection {pointFrom = [4,2], pointTo = [1,2,3], transition = "e"}
                                 , Connection {pointFrom = [1], pointTo = [4], transition = "implicit exit"}
                                 , Connection {pointFrom = [2], pointTo = [4], transition = "implicit exit"}
                                 , Connection {pointFrom = [1,1,2], pointTo = [3], transition = "c"}
                                 , Connection {pointFrom = [1,2,2], pointTo = [3], transition = "c"}
                                 , Connection {pointFrom = [1,1,1], pointTo = [1,1,2], transition = "a"}
                                 , Connection {pointFrom = [1,1,2], pointTo = [1,1,1], transition = "b"}
                                 , Connection {pointFrom = [1,2,1], pointTo = [1,2,2], transition = "b"}
                                 , Connection {pointFrom = [1,2,2], pointTo = [1,2,3], transition = "b"}
                                 , Connection {pointFrom = [1,2,3], pointTo = [1,2,1], transition = "b"}
                                 , Connection {pointFrom = [2,1], pointTo = [2,2], transition = "g"}
                                 , Connection {pointFrom = [2,2], pointTo = [2,1], transition = "g"}
                                 , Connection {pointFrom = [4,1], pointTo = [4,3], transition = "d"}
                                 , Connection {pointFrom = [4,3], pointTo = [4,2], transition = "d"}
                                 , Connection {pointFrom = [4,2], pointTo = [4,1], transition = "d"}]
                 , startState = [5]}

selfTransitionCase1Res1Step :: UMLStateDiagram [String] Int
selfTransitionCase1Res1Step
  = umlStateDiagram $
    StateDiagram { substates = [ StateDiagram { substates = [ InnerMostState {label = 1, name = ["J"], operations = ""}
                                                            , InnerMostState {label = 2, name = ["K"], operations = ""} ]
                                              , label = 1
                                              , name = ["E","G"]
                                              , connections = []
                                              , startState = [1]}
                               , InnerMostState {label = 2, name = ["E","F"], operations = ""}
                               , InnerMostState {label = 3, name = ["D"], operations = ""}
                               , StateDiagram { substates = [ InnerMostState {label = 2, name = ["B"], operations = ""}
                                                            , InnerMostState {label = 1, name = ["C"], operations = ""} ]
                                              , label = 4
                                              , name = ["A"]
                                              , connections = []
                                              , startState = [2]}
                               , EndState {label = 5}]
                 , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
                 , name = [""]
                 , connections = [ Connection {pointFrom = [3], pointTo = [5], transition = "dt"}
                                 , Connection {pointFrom = [1], pointTo = [1], transition = "lst"}
                                 , Connection {pointFrom = [2], pointTo = [1], transition = "lst"}
                                 , Connection {pointFrom = [1], pointTo = [3], transition = "gd"}
                                 , Connection {pointFrom = [3], pointTo = [1], transition = "de"}
                                 , Connection {pointFrom = [1,1], pointTo = [3], transition = "jd"}
                                 , Connection {pointFrom = [4,2], pointTo = [1,2], transition = "bk"}
                                 , Connection {pointFrom = [1], pointTo = [4], transition = "ea"}
                                 , Connection {pointFrom = [2], pointTo = [4], transition = "ea"}
                                 , Connection {pointFrom = [4,1], pointTo = [2], transition = "cf"}
                                 , Connection {pointFrom = [1], pointTo = [2], transition = "gf"}
                                 , Connection {pointFrom = [1], pointTo = [1], transition = "est"}
                                 , Connection {pointFrom = [1,2], pointTo = [1,1], transition = "kj"}
                                 , Connection {pointFrom = [4,2], pointTo = [4,1], transition = "bc"} ]
                 , startState = [1]}

spec :: Spec
spec
  = do
    describe "flatten tests" $ do
      it "flatten flatCase1" $ do
        let result = flatten flatCase1
        result `shouldBe` flatCase1Res
      it "flatten flatCase3 - lift one hierarchical state correctly" $ do
        let result = flatten flatCase3
        result `shouldBe` flatCase3Res1Step
      it "isStructurallySameAs" $ do
        let result = isStructurallySameAs flatCase1Res flatCase1Res
        result `shouldBe` True
      it "flatten flatCase7 - lift SD and retain root substates" $ do
        let result = flatten flatCase7
        connectionsOrderedByName result `shouldBe` connectionsOrderedByName flatCase7Res1Step
      it "flatten task26a - lift SD and retain root substates" $ do
        let result = flatten task26a
        result `shouldBe` task26aStep1Res
      it "flatten slide267b - lift SD with only InnerMostState children present in root" $ do
        let result = flatten slide267b
        result `shouldBe` slide267bRes1Step
      it "flatten task88 - lift SD and retain root substates" $ do
        let result = flatten task88
        result `shouldBe` task88Res1Step
      it "flatten noHistorySlide281 - lift SD with implicit exits" $ do
        let result = flatten noHistorySlide281
        result `shouldBe` noHistorySlide281Res1Step
      it "flatten test4 [step1] - lift SD and retain deeper layer structures of children integrated into the root" $ do
        let result = flatten test4
        result `shouldBe` test4Res1Step
      it "flatten noHistoryTest4 [step 1] - lift SD and retain deeper layer structures of children integrated into the root" $ do
        let result = flatten noHistoryTest4
        result `shouldBe` noHistoryTest4Res1Step
      it "flatten noHistoryTest4 [step 2] - lift SD and retain deeper layer structures of children integrated into the root" $ do
        let result = flatten' $ flatten noHistoryTest4
        result `shouldBe` noHistoryTest4Res2Step
      it "flatten noHistoryTest4 [step 3] - lift SD" $ do
        let result = flatten' $ flatten' $ flatten noHistoryTest4
        result `shouldBe` noHistoryTest4Res3Step
      it "flatten noHistoryTest4 [step 4] - lift SD" $ do
        let result = flatten' $ flatten' $ flatten' $ flatten noHistoryTest4
        result `shouldBe` noHistoryTest4Res4Step
      it "flatten implicitExitTask85 - lift SD and do not add connections for contained fork node" $ do
        let result = flatten' $ flatten implicitExitTask85
        result `shouldBe` implicitExitTask85Res1Step
      it "flatten selfTransitionCase1 - lift SD with a self transition" $ do
        let result = flatten selfTransitionCase1
        result `shouldBe` selfTransitionCase1Res1Step



isStructurallySameAs :: (Eq n, Eq l) => UMLStateDiagram n l -> UMLStateDiagram n l -> Bool
isStructurallySameAs g1 g2
  = length (unUML (\_ substate _ _ -> substate) g1) == length (unUML (\_ substate _ _ -> substate) g2)
