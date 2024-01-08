{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Modelling.StateDiagram.FlattenSpec (
  spec
) where

import Test.Hspec
import Modelling.StateDiagram.Datatype (StateDiagram(..)
                ,Connection (..)
                ,UMLStateDiagram
                ,umlStateDiagram
                ,unUML
                -- ,rename
                )
import Modelling.StateDiagram.Flatten (flatten)
import Modelling.StateDiagram.Checkers (allTheCheckers)
-- import System.Directory(createDirectoryIfMissing)
-- import Diagrams.Backend.SVG (renderSVG)
-- import Modelling.StateDiagram.Style (Styling(Unstyled))
-- import Modelling.StateDiagram.Layout (drawDiagram)
-- import Diagrams (dims)
-- import Diagrams.Prelude (V2(V2))

-- this chart should cover most cases within one instance
-- to avoid cluttering up the test suite with many overly
-- redundant charts
probeChart :: UMLStateDiagram String Int
probeChart
  = let
    dt = Connection {pointFrom = [2], pointTo = [1], transition = "dt"}
    gd = Connection {pointFrom = [3,1], pointTo = [2], transition = "gd"}
    de = Connection {pointFrom = [2], pointTo = [3], transition = "de"}
    jd = Connection {pointFrom = [3,1,1], pointTo = [2], transition = "jd"}
    kj = Connection {pointFrom = [3,1,2], pointTo = [3,1,1], transition = "kj"}
    lst = Connection {pointFrom = [3], pointTo = [3], transition = "lst"}
    gf = Connection {pointFrom = [3,1], pointTo = [3,2], transition = "gf"}
    est = Connection {pointFrom = [3,2], pointTo = [3,2], transition = "est"}
    enter = Connection {pointFrom = [3,2], pointTo = [5], transition = "enter"}
    in1 = Connection {pointFrom = [5], pointTo = [4,1,2], transition = ""}
    in2 = Connection {pointFrom = [5], pointTo = [4,2,2], transition = ""}
    ft = Connection {pointFrom = [3,2], pointTo = [1], transition = "ft"}
    exit = Connection {pointFrom = [6], pointTo = [3,1,2], transition = ""}
    ex1 = Connection {pointFrom = [4,1,1], pointTo = [6], transition = "exit"}
    ex2 = Connection {pointFrom = [4,2,2], pointTo = [6], transition = "exit"}
    cb = Connection {pointFrom = [4,1,1], pointTo = [4,1,2], transition = "cb"}
    bc = Connection {pointFrom = [4,1,2], pointTo = [4,1,1], transition = "bc"}
    ml = Connection {pointFrom = [4,2,1], pointTo = [4,2,2], transition = "ml"}
    lm = Connection {pointFrom = [4,2,2], pointTo = [4,2,1], transition = "lm"}
    eCD = Connection {pointFrom = [3], pointTo = [4], transition = "eCD"}
    kl = Connection {pointFrom = [3,1,2], pointTo = [4,2,2], transition = "kl"}
    cst = Connection {pointFrom = [4,2,1], pointTo = [4,2,1], transition = "cst"}
    in
    umlStateDiagram $
    StateDiagram { substates = [fs,isD,sdE,cd,fork,join]
                 , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
                 , name = ""
                 , connections = [dt,gd,de,jd,kj,lst
                                 ,gf,est,enter,in1,in2
                                 ,ft,exit,ex1,ex2,cb,bc
                                 ,ml,lm,eCD,kl,cst]
                 , startState = [3]}
    where
    fs = EndState { label = 1 }
    isD = InnerMostState { label = 2
                         , name = "D"
                         , operations = "" }
    fork = Fork { label = 5 }
    join = Join { label = 6 }
    isB = InnerMostState { label = 1
                         , name = "B"
                         , operations = "" }
    isC = InnerMostState { label = 2
                         , name = "C"
                         , operations = "" }
    sd1 = StateDiagram { substates = [isB, isC]
                       , label = 1, name = ""
                       , connections = []
                       , startState = [1] }
    isM = InnerMostState { label = 1
                         , name = "M"
                         , operations = "" }
    isL = InnerMostState { label = 2
                         , name = "L"
                         , operations = "" }
    sd2 = StateDiagram { substates = [isM, isL]
                       , label = 2, name = ""
                       , connections = []
                       , startState = [1] }
    cd = CombineDiagram { substates = [sd1, sd2]
                        , label = 4 }
    isJ = InnerMostState { label = 1
                         , name = "J"
                         , operations = "" }
    isK = InnerMostState { label = 2
                         , name = "K"
                         , operations = "" }
    sdG = StateDiagram { substates = [isJ, isK]
                       , label = 1
                       , name = "G"
                       , connections = []
                       , startState = [1] }
    isF = InnerMostState { label = 2
                         , name = "F"
                         , operations = "" }
    sdE = StateDiagram { substates = [sdG, isF]
                       , label = 3, name = "E"
                       , connections = []
                       , startState = [1] }

probeChartLiftedSDeAndSDg :: UMLStateDiagram [String] Int
probeChartLiftedSDeAndSDg
  = umlStateDiagram $
    StateDiagram { substates = [ InnerMostState { label = 1
                                                , name = ["E","G","J"]
                                                , operations = ""}
                               , InnerMostState { label = 2
                                                , name = ["E","G","K"]
                                                , operations = ""}
                               , InnerMostState { label = 3
                                                , name = ["E","F"]
                                                , operations = ""}
                               , EndState {label = 4}
                               , InnerMostState { label = 5
                                                , name = ["D"]
                                                , operations = ""}
                               , CombineDiagram { substates = [ StateDiagram { substates = [ InnerMostState { label = 1
                                                                                                            , name = ["B"]
                                                                                                            , operations = "" }
                                                                                           , InnerMostState { label = 2
                                                                                                            , name = ["C"]
                                                                                                            , operations = "" }
                                                                                           ]
                                                                             , label = 1
                                                                             , name = [""]
                                                                             , connections = []
                                                                             , startState = [1]
                                                                             }
                                                             , StateDiagram { substates = [ InnerMostState { label = 1
                                                                                                           , name = ["M"]
                                                                                                           , operations = "" }
                                                                                          , InnerMostState { label = 2
                                                                                                           , name = ["L"]
                                                                                                           , operations = "" }
                                                                                          ]
                                                                            , label = 2
                                                                            , name = [""]
                                                                            , connections = []
                                                                            , startState = [1]
                                                                            }
                                                              ]
                                                 , label = 6 }
                               , Fork {label = 7}
                               , Join {label = 8}
                               ]
                 , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
                 , name = [""]
                 , connections = [ Connection {pointFrom = [5], pointTo = [4], transition = "dt"}
                                 , Connection {pointFrom = [1], pointTo = [5], transition = "gd"}
                                 , Connection {pointFrom = [2], pointTo = [5], transition = "gd"}
                                 , Connection {pointFrom = [5], pointTo = [1], transition = "de"}
                                 , Connection {pointFrom = [1], pointTo = [5], transition = "jd"}
                                 , Connection {pointFrom = [2], pointTo = [1], transition = "kj"}
                                 , Connection {pointFrom = [1], pointTo = [1], transition = "lst"}
                                 , Connection {pointFrom = [2], pointTo = [1], transition = "lst"}
                                 , Connection {pointFrom = [3], pointTo = [1], transition = "lst"}
                                 , Connection {pointFrom = [1], pointTo = [3], transition = "gf"}
                                 , Connection {pointFrom = [2], pointTo = [3], transition = "gf"}
                                 , Connection {pointFrom = [3], pointTo = [3], transition = "est"}
                                 , Connection {pointFrom = [3], pointTo = [7], transition = "enter"}
                                 , Connection {pointFrom = [7], pointTo = [6,1,2], transition = ""}
                                 , Connection {pointFrom = [7], pointTo = [6,2,2], transition = ""}
                                 , Connection {pointFrom = [3], pointTo = [4], transition = "ft"}
                                 , Connection {pointFrom = [8], pointTo = [2], transition = ""}
                                 , Connection {pointFrom = [6,1,1], pointTo = [8], transition = "exit"}
                                 , Connection {pointFrom = [6,2,2], pointTo = [8], transition = "exit"}
                                 , Connection {pointFrom = [6,1,1], pointTo = [6,1,2], transition = "cb"}
                                 , Connection {pointFrom = [6,1,2], pointTo = [6,1,1], transition = "bc"}
                                 , Connection {pointFrom = [6,2,1], pointTo = [6,2,2], transition = "ml"}
                                 , Connection {pointFrom = [6,2,2], pointTo = [6,2,1], transition = "lm"}
                                 , Connection {pointFrom = [1], pointTo = [6], transition = "eCD"}
                                 , Connection {pointFrom = [2], pointTo = [6], transition = "eCD"}
                                 , Connection {pointFrom = [3], pointTo = [6], transition = "eCD"}
                                 , Connection {pointFrom = [2], pointTo = [6,2,2], transition = "kl"}
                                 , Connection {pointFrom = [6,2,1], pointTo = [6,2,1], transition = "cst"}
                                 ]
                 , startState = [1]}

-- for code coverage reasons (executing all paths)
-- it can make sense to alter the start state of the diagram
-- as the lifted state might or might not be a global start state
withInitialState :: [Int] -> UMLStateDiagram n Int -> UMLStateDiagram n Int
withInitialState initial chart
  = umlStateDiagram
    $ unUML (\rootName rootSubstates rootConnections _ ->
        StateDiagram { name = rootName
                     , substates = rootSubstates
                     , connections = rootConnections
                     , startState = initial
                     , label = error "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!" } )
    chart

spec :: Spec
spec
  = do
    describe "flatten tests" $ do
      it "probe chart satisfies Haskell chart checkers" $ do
        let result = map (($ probeChart) . snd) allTheCheckers
        all (Nothing ==) result `shouldBe` True
      it "flatten - lift SD(E) and then SD(G) of probeChart" $ do
        let result = flatten probeChart
        result `shouldBe` probeChartLiftedSDeAndSDg
      it "flatten - lift SD(E) and then SD(G) of probeChart with initial state set to D instead of SD(E)" $ do
        let result = flatten $ withInitialState [2] probeChart
        result `shouldBe` withInitialState [5] probeChartLiftedSDeAndSDg
      it "flatten all hierarchical states" $ do
      --  let result = flatten probeChart
      --  result `shouldBe` ...
        pendingWith "is only possible after scenario2 extension is implemented"
      it "print flat and non-flat probeChart to /temp/*.svg files using internal renderer" $ do
        --createDirectoryIfMissing True "./temp"
        --renderSVG "./temp/probeChart.svg" (dims (V2 800 600)) (drawDiagram Unstyled probeChart)
        --renderSVG "./temp/....svg" (dims (V2 800 600)) (drawDiagram Unstyled $ rename concat (flatten probeChart))
        --return ()
        pendingWith "disabled until scenario2 extension is implemented"
