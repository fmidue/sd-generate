module Modelling.StateDiagram.CounterExample where
import Modelling.StateDiagram.Datatype (
  Connection(..),
  HistoryType(..),
  StateDiagram(..),
  UMLStateDiagram,
  umlStateDiagram,
  )

{-# ANN module "Hlint: ignore Reduce duplication" #-}

forCheckReachability1 :: UMLStateDiagram String Int
forCheckReachability1
  = umlStateDiagram $
    StateDiagram [a, b, c, d] 1 ""
    [ Connection [1] [2] "a"
    , Connection [1] [3] "b"
    , Connection [4] [3] "d"
    , Connection [3] [1] "e"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""
    c = InnerMostState 3 "C" ""
    d = InnerMostState 4 "D" ""

forCheckReachability2 :: UMLStateDiagram String Int
forCheckReachability2
  = umlStateDiagram $
    StateDiagram [a, b, c, d] 1 ""
    [ Connection [1, 1, 3] [2] "e"
    , Connection [1, 2, 2] [2] "e"
    , Connection [2] [3] ""
    , Connection [3] [4] "c"
    , Connection [3] [4, 2] "d"
    , Connection [4, 2] [1, 2, 2] "a"
    , Connection [4, 1] [1] "a" ] [1, 1, 2]
  where
    a = CombineDiagram [e, f] 1
      where
        e = StateDiagram [i, j, k] 1 ""
            [ Connection [1] [2] "a"
            , Connection [2] [3] "b"
            , Connection [3] [1] "a" ] [1]
          where
            i = InnerMostState 1 "A" ""
            j = InnerMostState 2 "B" ""
            k = InnerMostState 3 "C" ""
        f = StateDiagram [l, m] 2 ""
            [ Connection [1] [2] "b"
            , Connection [2] [1] "c" ] [1]
          where
            l = InnerMostState 1 "D" ""
            m = InnerMostState 2 "E" ""
    b = Join 2
    c = InnerMostState 3 "F" ""
    d = StateDiagram [g, h] 4 "" [] [2]
      where
        g = InnerMostState 1 "G" ""
        h = InnerMostState 2 "H" ""

forCheckReachability3 :: UMLStateDiagram String Int
forCheckReachability3
  = umlStateDiagram $
    StateDiagram [a] 1 "" [] []
  where
    a = InnerMostState 1 "A" ""

bogusExample :: UMLStateDiagram String Int
bogusExample
  = umlStateDiagram $
    StateDiagram [ CombineDiagram [a,b] 1]  0 ""
    [ Connection [1,2,0] [1,3,0] ""
    , Connection [1] [1,2,0] "" ] []
  where
    a = StateDiagram [InnerMostState 0 "" ""] 2 "" [] []
    b = StateDiagram [InnerMostState 0 "" ""] 3 "" [] []

foCheckCrossings1 :: UMLStateDiagram String Int
foCheckCrossings1
  = umlStateDiagram $
    StateDiagram [ CombineDiagram [a,b] 1]  0 ""
    [ Connection [1,2,0] [1,3,0] "" ] [1,2,0]
    where
      a = StateDiagram [ InnerMostState 0 "" "" ] 2 "" [] []
      b = StateDiagram [ InnerMostState 0 "" "" ] 3 "" [] []

forCheckTransition1 :: UMLStateDiagram String Int
forCheckTransition1
  = umlStateDiagram $
    StateDiagram [a,b,c,d,e,f,g] 1 ""
    [ Connection [5] [1] "a"
    , Connection [1] [2] "a"
    , Connection [2] [3] ""
    , Connection [3] [6] ""
    , Connection [5] [4] ""
    , Connection [4] [6] ""
    , Connection [6] [7] "" ] [5]
  where a = InnerMostState 1 "Tasse nehmen" ""
        b = InnerMostState 2 "Kaffee trinken" ""
        c = InnerMostState 3 "Tasse absetzen" ""
        d = InnerMostState 4 "Zeitung lesen" ""
        e = Fork 5
        f = Join 6
        g = EndState 7

forCheckTransition2 :: UMLStateDiagram String Int
forCheckTransition2
  = umlStateDiagram $
    StateDiagram [a,b,c,d,e,f,g] 1 ""
    [ Connection [5] [1] "a"
    , Connection [1] [2] "a"
    , Connection [2] [3] ""
    , Connection [3] [6] "a"
    , Connection [5] [4] "a"
    , Connection [4] [6] ""
    , Connection [6] [7] "" ] [5]
  where a = InnerMostState  1 "Tasse nehmen" ""
        b = InnerMostState  2 "Kaffee trinken" ""
        c = InnerMostState  3 "Tasse absetzen" ""
        d = InnerMostState  4 "Zeitung lesen" ""
        e = Fork 5
        f = Join 6
        g = EndState 7

forCheckManyToOne2 :: UMLStateDiagram String Int
forCheckManyToOne2
  = umlStateDiagram $
    StateDiagram [a,b,c,d,e] 1 ""
    [ Connection [1] [5] "c"
    , Connection [5] [4] ""
    , Connection [2] [1] "a"
    , Connection [1] [2] "a"
    , Connection [1] [3] "b" ] [5]
  where a = InnerMostState  1 "A" ""
        b = InnerMostState  2 "B" ""
        c = InnerMostState  3 "C" ""
        d = InnerMostState  4 "D" ""
        e = Fork 5 -- ambiguous interpretations possible (and all wrong)

forCheckManyToOne1 :: UMLStateDiagram String Int
forCheckManyToOne1
  = umlStateDiagram $
    StateDiagram [a,b,c,d,e] 1 ""
    [ Connection [2] [5] "b"
    , Connection [1] [5] ""
    , Connection [5] [4] "a"
    , Connection [5] [3] "a"
    , Connection [2] [1] "a" ] [2]
  where a = InnerMostState  1 "A" ""
        b = InnerMostState  2 "B" ""
        c = InnerMostState  3 "C" ""
        d = InnerMostState  4 "D" ""
        e = Fork 5 -- ambiguous interpretations possible (and all wrong)

forCheckManyToOne3 :: UMLStateDiagram String Int
forCheckManyToOne3
  = umlStateDiagram $
    StateDiagram [a,b,c,d,e] 1 ""
    [ Connection [5] [4] ""
    , Connection [2] [1] "a"
    , Connection [1] [2] "a"
    , Connection [1] [3] "b" ] [5]
  where a = InnerMostState  1 "A" ""
        b = InnerMostState  2 "B" ""
        c = InnerMostState  3 "C" ""
        d = InnerMostState  4 "D" ""
        e = Fork 5 -- ambiguous interpretations possible (and all wrong)

forCheckManyToOne5 :: UMLStateDiagram String Int
forCheckManyToOne5
  = umlStateDiagram $
    StateDiagram [a,b,c,d,e] 1 ""
    [ Connection [4] [5] ""
    , Connection [3] [5] ""
    , Connection [2] [1] "a"
    , Connection [1] [2] "a"
    , Connection [1] [3] "b" ] [4]
  where a = InnerMostState  1 "A" ""
        b = InnerMostState  2 "B" ""
        c = InnerMostState  3 "C" ""
        d = InnerMostState  4 "D" ""
        e = Join 5

forCheckManyToOne6 :: UMLStateDiagram String Int
forCheckManyToOne6
  = umlStateDiagram $
    StateDiagram [a,b,c,d,e] 1 ""
    [ Connection[4] [5] ""
    , Connection[5] [2] ""
    , Connection[2] [1] "a"
    , Connection[1] [4] "a"
    , Connection[1] [3] "b" ] []
  where a = InnerMostState  1 "A" ""
        b = InnerMostState  2 "B" ""
        c = InnerMostState  3 "C" ""
        d = InnerMostState  4 "D" ""
        e = Fork 5 -- ambiguous interpretations possible (and all wrong)

forCheckManyToOne7 :: UMLStateDiagram String Int
forCheckManyToOne7
  = umlStateDiagram $
    StateDiagram [a, b, c, d, l] 1 ""
    [ Connection [2, 1, 3] [3] ""
    , Connection [2, 2, 2] [3] ""
    , Connection [3] [4] "g"
    , Connection [3] [5] "g" ] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, i] 1 ""
            [ Connection [1] [2] "a"
            , Connection [1] [3] "b" ] [1]
          where
            g = InnerMostState 1 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
        f = StateDiagram [j, k] 2 ""
            [ Connection [1] [2] "a" ] [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    c = Fork 3
    d = InnerMostState 4 "G" ""
    l = EndState 5

forCheckManyToOne8 :: UMLStateDiagram String Int
forCheckManyToOne8
  = umlStateDiagram $
    StateDiagram [a,b,c,d,e] 1 ""
    [ Connection [2] [1] "a"
    , Connection [1] [4] "a"
    , Connection [1] [3] "b"
    , Connection [1] [2] "c" ] [5]
  where a = InnerMostState  1 "A" ""
        b = InnerMostState  2 "B" ""
        c = InnerMostState  3 "C" ""
        d = InnerMostState  4 "D" ""
        e = Fork 5

forCheckManyToOne4 :: UMLStateDiagram String Int
forCheckManyToOne4
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection [5] [1,1,1] "" ] [5]
  where
    a = CombineDiagram [e,f] 1
      where
        e = StateDiagram [ InnerMostState 1 "A" "" ] 1 "" [] []
        f = StateDiagram [ InnerMostState 1 "A" "" ] 2 "" [] [1]
    b = Fork 5

forCheckTransition3 :: UMLStateDiagram String Int
forCheckTransition3
  = umlStateDiagram $
    StateDiagram [a, b, d, l] 1 ""
    [ Connection [2] [2, 1, 4] "a"
    , Connection [4] [2, 1, 4] "b"
    , Connection [4] [5] "a"
    , Connection [2] [4] "b" ] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, c, i] 1 ""
            [ Connection [4] [2] "c"
            , Connection [3] [2] "b"
            , Connection [2] [3] "b" ] [1]
          where
            g = InnerMostState 1 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
            c = Join 4 -- cant be semantically correct anyway
        f = StateDiagram [j, k] 2 ""
            [ Connection [1] [2] "b" ] [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    d = InnerMostState 4 "G" ""
    l = EndState 5

forCheckTransition4 :: UMLStateDiagram String Int
forCheckTransition4
  = umlStateDiagram $
    StateDiagram [a,b,c,d,e] 1 ""
    [ Connection [5] [4] "a"
    , Connection [5] [2] "c"
    , Connection [4] [3] "a"
    , Connection [4] [1] "b" ] [5]
  where a = InnerMostState  1 "A" ""
        b = InnerMostState  2 "B" ""
        c = InnerMostState  3 "C" ""
        d = InnerMostState  4 "D" ""
        e = Fork 5

forCheckTransition5 :: UMLStateDiagram String Int
forCheckTransition5
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection [5] [1,1,1] "a"
    , Connection [5] [1,2,1] "b" ] [5]
  where
    a = CombineDiagram [e,f] 1
      where
        e = StateDiagram [ InnerMostState 1 "A" "" ] 1 "" [] []
        f = StateDiagram [ InnerMostState 1 "A" "" ] 2 "" [] []
    b = Fork 5

forAllGoIntoParallelRegions1 :: UMLStateDiagram String Int
forAllGoIntoParallelRegions1
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection [5] [1,1,1] "a"
    , Connection [5] [1,1,2] "a" ] [5]
  where
    a = CombineDiagram [e,f] 1
      where
        e = StateDiagram
            [ InnerMostState 1 "A" ""
            , InnerMostState 2 "B" "" ] 1 "" [] []
        f = StateDiagram
            [ InnerMostState 1 "A" "" ] 2 "" [] [1]
    b = Fork 5

forAllGoIntoParallelRegions2 :: UMLStateDiagram String Int
forAllGoIntoParallelRegions2
  = umlStateDiagram $
    StateDiagram [a,b,g] 1 ""
    [ Connection [5] [1,1,1] "a"
    , Connection [5] [2,1,1] "a" ] [5]
  where
    a = CombineDiagram [e,f] 1
      where
        e = StateDiagram [ InnerMostState 1 "A" "" ] 1 "" [] []
        f = StateDiagram [ InnerMostState 1 "A" "" ] 2 "" [] [1]
    b = CombineDiagram [c,d] 2
      where
        c = StateDiagram [ InnerMostState 1 "A" "" ] 1 "" [] []
        d = StateDiagram [ InnerMostState 1 "A" "" ] 2 "" [] [1]
    g = Fork 5

forAllComeOutOfParallelRegions1 :: UMLStateDiagram String Int
forAllComeOutOfParallelRegions1
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection [1,1,1] [5] "a"
    , Connection [1,1,2] [5] "a"
    , Connection[5] [1,2,1] "a" ] [1,1,2]
  where
    a = CombineDiagram [e,f] 1
      where
        e = StateDiagram
            [ InnerMostState 1 "A" ""
            , InnerMostState 2 "B" "" ] 1 "" [] [1]
        f = StateDiagram
            [ InnerMostState 1 "A" "" ] 2 "" [] []
    b = Join 5

forCheckEndState1 :: UMLStateDiagram String Int
forCheckEndState1
  = umlStateDiagram $
    StateDiagram [b,e] 1 ""
    [ Connection[7] [2] "" ] [7]
  where
    b = InnerMostState  2 "Kaffee trinken" ""
    e = EndState 7

forCheckEndState2 :: UMLStateDiagram String Int
forCheckEndState2
  = umlStateDiagram $
    StateDiagram [a, b] 1 ""
    [ Connection [1,3] [2] "a" ] []
  where
    a = StateDiagram [c, d, e] 1 "A"
        [ Connection [1] [2] ""
        , Connection [2] [3] "b"
        , Connection [3] [2] "b"
        , Connection [2] [1] "a" ] [1]
      where
        c = InnerMostState 1 "B" ""
        d = InnerMostState 2 "C" ""
        e = EndState 3
    b = InnerMostState 2 "E" ""

forCheckEndState3 :: UMLStateDiagram String Int
forCheckEndState3
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection[2] [1] "" ] [2]
  where
    a = InnerMostState  1 "A" ""
    b = EndState 2

forCheckSubS1 :: UMLStateDiagram String Int
forCheckSubS1
  = umlStateDiagram $
    StateDiagram
    [ InnerMostState 1 "A" ""
    , InnerMostState 2 "B" "" ]
    1 ""
    [ Connection [1] [2] ""
    , Connection [2] [1] "" ] [1,2]

forCheckSubS2 :: UMLStateDiagram String Int
forCheckSubS2
  = umlStateDiagram $
    StateDiagram
    [ CombineDiagram [a,b,c] 1 ] 1 "active" [] [1,5,2]
  where
    a = StateDiagram [d,e] 1 ""
        [ Connection [1] [2] "EvNumLockPressed"
        , Connection [2] [1] "EvNumLockPressed" ] [1]
      where d = InnerMostState 1 "NumLockOff" ""
            e = InnerMostState 2 "NumLockOn" ""
    b = StateDiagram [f,g] 2 ""
        [ Connection [1] [2] "EvCapsLockPressed"
        , Connection [2] [1] "EvCapsLockPressed" ] [1]
      where f = InnerMostState 1 "CapsLockOff" ""
            g = InnerMostState 2 "CapsLockOn" ""
    c = StateDiagram [h,i] 3 ""
        [ Connection [1] [2] "EvScrollLockPressed"
        , Connection [2] [1] "EvScrollLockPressed" ] [1]
      where h = InnerMostState 1 "ScrollLockOff" ""
            i = InnerMostState 2 "ScrollLockOn" ""

forCheckSubS3 :: UMLStateDiagram String Int
forCheckSubS3
  = umlStateDiagram $
    StateDiagram [ CombineDiagram [a,b,c] 1 ] 1 "active" [] [5]
  where
    a = StateDiagram  [d,e] 1 ""
        [ Connection [1] [2] "EvNumLockPressed"
        , Connection [2] [1] "EvNumLockPressed" ] [1]
      where d = InnerMostState 1 "NumLockOff" ""
            e = InnerMostState 2 "NumLockOn" ""
    b = StateDiagram  [f,g] 2 ""
        [ Connection [1] [2] "EvCapsLockPressed"
        , Connection [2] [1] "EvCapsLockPressed" ] [1]
      where f = InnerMostState 1 "CapsLockOff" ""
            g = InnerMostState 2 "CapsLockOn" ""
    c = StateDiagram  [h,i] 3 ""
        [ Connection [1] [2] "EvScrollLockPressed"
        , Connection [2] [1] "EvScrollLockPressed" ] [1]
      where h = InnerMostState 1 "ScrollLockOff" ""
            i = InnerMostState 2 "ScrollLockOn" ""

forCheckSubS4 :: UMLStateDiagram String Int
forCheckSubS4
  = umlStateDiagram $ StateDiagram [a,b] 1 ""
    [ Connection[1] [2] "t"
    , Connection[2] [1,3] "" ] []
  where
    a = StateDiagram  [c,d,e] 1 "Composite State"
        [ Connection [2] [1] "" ] [5]
        where
          c = InnerMostState  1 "State 1" ""
          d = StateDiagram  [f,g] 2 "state 2"
              [ Connection [1] [2] "" ] [1]
           where
             f = InnerMostState  1 "State 2a" ""
             g = InnerMostState  2 "State 2b" ""
          e = History 3 Deep
    b = InnerMostState  2 "State 3" ""

forCheckSubS5 :: UMLStateDiagram String Int
forCheckSubS5
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection[1] [2] "t"
    , Connection[2] [1,3] "" ] []
  where
    a = StateDiagram  [c,d,e] 1 "Composite State"
        [ Connection [2] [1] "" ] [2,1,3]
        where
          c = InnerMostState  1 "State 1" ""
          d = StateDiagram  [f,g] 2 "state 2"
              [ Connection [1] [2] "" ] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
          e = History 3 Deep
    b = InnerMostState  2 "State 3" ""

forCheckSubS6 :: UMLStateDiagram String Int
forCheckSubS6
  = umlStateDiagram $
    StateDiagram [a, b, c, d] 1 ""
    [ Connection [1] [2] "a"
    , Connection [2, 1, 2] [4] "h"
    , Connection [2, 1, 3] [3] "a"
    , Connection [2, 2, 2] [3] ""
    , Connection [3] [4] "g" ] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, i] 1 ""
            [ Connection [1] [2] "b"
            , Connection [3] [1] "b"
            , Connection [2] [3] "c" ] [5]
          where
            g = InnerMostState 1 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
        f = StateDiagram [j, k] 2 "" [ Connection [1] [2] "e" ] [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    c = InnerMostState 3 "H" ""
    d = InnerMostState 4 "G" ""

forCheckStartToRegion1 ::UMLStateDiagram String Int
forCheckStartToRegion1
  = umlStateDiagram $
    StateDiagram
    [ CombineDiagram [a,b] 1
    , InnerMostState 2 "" "" ]
    0 ""
    [ Connection [1,3,0] [2] "" ] [1,3]
    where
      a = StateDiagram [ InnerMostState 0 "" "" ] 3 "" [] [0]
      b = StateDiagram [ InnerMostState 0 "" "" ] 4 "" [] [0]

forCheckStartToRegion2 ::UMLStateDiagram String Int
forCheckStartToRegion2
  = umlStateDiagram $
    StateDiagram
    [ CombineDiagram [a,b] 1
    , InnerMostState  2 "" ""]
    0 ""
    [ Connection [1] [2] "" ] []
    where
      a = StateDiagram [c,d] 3 ""
          [ Connection [1] [2] "" ] [1,1]
        where
          c = CombineDiagram [e,f] 1
            where
              e = StateDiagram [ InnerMostState 0 "" "" ] 1 "" [] [0]
              f = StateDiagram [ InnerMostState 0 "" "" ] 2 "" [] [0]
          d = InnerMostState  2 "" ""
      b = StateDiagram [ InnerMostState 0 "" "" ] 4 "" [] [0]

forCheckConnection1 :: UMLStateDiagram String Int
forCheckConnection1
  = umlStateDiagram $
    StateDiagram [a, b] 1 ""
    [ Connection [5] [1] "a" ] [2]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""

forCheckConnection2 :: UMLStateDiagram String Int
forCheckConnection2
  = umlStateDiagram $
    StateDiagram [a, b, c, d] 1 ""
    [ Connection [1] [2] "a"
    , Connection [2, 1, 2] [4] "h"
    , Connection [2, 1, 4] [3] ""
    , Connection [2, 2, 2] [3] ""
    , Connection [3] [4] "g" ] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, i] 1 ""
            [ Connection [1] [2] "b"
            , Connection [2] [3] "c"] [1]
          where
            g = InnerMostState 1 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
        f = StateDiagram [j, k] 2 ""
            [ Connection [1] [2] "e" ] [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    c = Join 3
    d = InnerMostState 4 "G" ""

forCheckConnection3 :: UMLStateDiagram String Int
forCheckConnection3
  = umlStateDiagram $
    StateDiagram [a, b, c, d] 1 ""
    [ Connection [1] [2] "a"
    , Connection [2, 1, 2] [4] "h"
    , Connection [2, 4, 3] [3] ""
    , Connection [2, 2, 2] [3] ""
    , Connection [3] [4] "g"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, i] 1 ""
            [ Connection [1] [2] "b"
            , Connection [2] [3] "c"] [1]
          where
            g = InnerMostState 1 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
        f = StateDiagram [j, k] 2 ""
            [ Connection [1] [2] "e" ] [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    c = Join 3
    d = InnerMostState 4 "G" ""

forCheckConnection4 :: UMLStateDiagram String Int
forCheckConnection4
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection[1] [2] "t"
    , Connection[2] [1,3] "" ] []
  where
    a = StateDiagram  [c,d,e] 1 "Composite State"
        [ Connection [5] [2] "" ] [1]
      where
        c = InnerMostState  1 "State 1" ""
        d = StateDiagram  [f,g] 2 "state 2"
            [ Connection [1] [2] "" ] [1]
          where
            f = InnerMostState  1 "State 2a" ""
            g = InnerMostState  2 "State 2b" ""
        e = History 3 Deep
    b = InnerMostState  2 "State 3" ""

forCheckConnection5::UMLStateDiagram String Int
forCheckConnection5
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection[1] [2] "t"
    , Connection[2] [1,3] "" ] []
  where
    a = StateDiagram  [c,d,e] 1 "Composite State"
        [ Connection [1] [2] "" ] [1]
      where
        c = InnerMostState  1 "State 1" ""
        d = StateDiagram  [f,g] 2 "state 2"
            [ Connection [1] [2] "a"
            , Connection [1] [99] "b"] [1]
          where
            f = InnerMostState  1 "State 2a" ""
            g = InnerMostState  2 "State 2b" ""
        e = History 3 Deep
    b = InnerMostState  2 "State 3" ""

forCheckConnection6::UMLStateDiagram String Int
forCheckConnection6
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection[1] [2] "t"
    , Connection[2] [1,3] "" ] []
  where
    a = StateDiagram  [c,d,e] 1 "Composite State"
        [ Connection [1] [2] "" ] [1]
      where
        c = InnerMostState  1 "State 1" ""
        d = StateDiagram  [f,g] 2 "state 2"
            [ Connection [1] [2] "a"
            , Connection [1] [0] "b" ] [1]
          where
            f = InnerMostState  1 "State 2a" ""
            g = InnerMostState  2 "State 2b" ""
        e = History 3 Deep
    b = InnerMostState  2 "State 3" ""


forCheckConnection7 :: UMLStateDiagram String Int
forCheckConnection7
  = umlStateDiagram $
    StateDiagram
    [ CombineDiagram [a,b,c] 1] 1 "active" [] [1,2,2]
  where
    a = StateDiagram  [d,e] 1 ""
        [ Connection [4] [2] "EvNumLockPressed"
        , Connection [2] [1] "EvNumLockPressed" ] [1]
      where d = InnerMostState 1 "NumLockOff" ""
            e = InnerMostState 2 "NumLockOn" ""
    b = StateDiagram  [f,g] 2 ""
        [ Connection [1] [2] "EvCapsLockPressed"
        , Connection [2] [1] "EvCapsLockPressed" ] [1]
      where f = InnerMostState 1 "CapsLockOff" ""
            g = InnerMostState 2 "CapsLockOn" ""
    c = StateDiagram  [h,i] 3 ""
        [ Connection [1] [2] "EvScrollLockPressed"
        , Connection [2] [1] "EvScrollLockPressed" ] [1]
      where h = InnerMostState 1 "ScrollLockOff" ""
            i = InnerMostState 2 "ScrollLockOn" ""

forCheckConnection8 :: UMLStateDiagram String Int
forCheckConnection8
  = umlStateDiagram $
    StateDiagram [ CombineDiagram [a,b,c] 1 ] 1 "active" [] [1,2,2]
  where
   a = StateDiagram  [d,e] 1 ""
       [ Connection [2,3] [2] "EvNumLockPressed"
       , Connection [2] [1] "EvNumLockPressed" ] [1]
     where d = InnerMostState 1 "NumLockOff" ""
           e = InnerMostState 2 "NumLockOn" ""
   b = StateDiagram  [f,g] 2 ""
       [ Connection [1] [2] "EvCapsLockPressed"
       , Connection [2] [1] "EvCapsLockPressed" ] [1]
     where f = InnerMostState 1 "CapsLockOff" ""
           g = InnerMostState 2 "CapsLockOn" ""
   c = StateDiagram  [h,i] 3 ""
       [ Connection [1] [2] "EvScrollLockPressed"
       , Connection [2] [1] "EvScrollLockPressed" ] [1]
     where h = InnerMostState 1 "ScrollLockOff" ""
           i = InnerMostState 2 "ScrollLockOn" ""

forCheckConnFromToRegion1 ::UMLStateDiagram String Int
forCheckConnFromToRegion1
  = umlStateDiagram $
    StateDiagram
    [ CombineDiagram [a,b] 1
    , InnerMostState  2 "" "" ] 0 ""
    [ Connection [1,3] [2] "" ] []
  where
    a = StateDiagram [InnerMostState 0 "" ""] 3 "" [] [0]
    b = StateDiagram [InnerMostState 0 "" ""] 4 "" [] [0]

forCheckConnFromToRegion2 ::UMLStateDiagram String Int
forCheckConnFromToRegion2
  = umlStateDiagram $
    StateDiagram
    [ CombineDiagram [a,b] 1
    , InnerMostState  2 "" "" ] 0 ""
    [ Connection [2] [1,3] "" ] [2]
  where
    a = StateDiagram [ InnerMostState 0 "" "" ] 3 "" [] [0]
    b = StateDiagram [ InnerMostState 0 "" "" ] 4 "" [] [0]

forCheckConnFromToRegion3 ::UMLStateDiagram String Int
forCheckConnFromToRegion3
  = umlStateDiagram $
    StateDiagram
    [ CombineDiagram [a,b] 1
    , InnerMostState  2 "" "" ] 0 "" [] [2]
  where
    a = StateDiagram [c,d] 3 "" [ Connection [1,1] [2] "" ] []
      where
        c = CombineDiagram [e,f] 1
          where
              e = StateDiagram [ InnerMostState 0 "" "" ] 1 "" [] [0]
              f = StateDiagram [ InnerMostState 0 "" "" ] 2 "" [] [0]
        d = InnerMostState  2 "" ""
    b = StateDiagram [ InnerMostState 0 "" "" ] 4 "" [] [0]

forCheckConnFromToRegion4 ::UMLStateDiagram String Int
forCheckConnFromToRegion4
  = umlStateDiagram $
    StateDiagram
    [ CombineDiagram [a,b] 1
    , InnerMostState  2 "" ""] 0 ""
    [ Connection [1,3,1,1] [2] "" ] []
  where
    a = StateDiagram [c,d] 3 "" [] [2]
      where
        c = CombineDiagram [e,f] 1
          where
              e = StateDiagram [ InnerMostState 0 "" "" ] 1 "" [] [0]
              f = StateDiagram [ InnerMostState 0 "" "" ] 2 "" [] [0]
        d = InnerMostState  2 "" ""
    b = StateDiagram [ InnerMostState 0 "" "" ] 4 "" [] [0]


forCheckNameUniqueness1 :: UMLStateDiagram String Int
forCheckNameUniqueness1
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection[1] [2] "t"
    , Connection[2] [1,3] "" ] []
  where
    a = StateDiagram  [c,d,e] 1 "Composite State"
        [ Connection [1] [2] "" ] [1]
        where
          c = InnerMostState  1 "a" ""
          d = StateDiagram  [f,g] 2 "a"
              [ Connection [1] [2] "" ] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
          e = History 3 Deep
    b = InnerMostState  2 "State 3" ""

forCheckNameUniqueness2 :: UMLStateDiagram String Int
forCheckNameUniqueness2
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection [1] [2] "t"
    , Connection [2] [1,3] "" ] []
  where
   a = StateDiagram  [c,d,e] 1 "a"
       [ Connection [1] [2] "" ] [1]
     where
      c = InnerMostState  1 "a" ""
      d = StateDiagram  [f,g] 2 "State 2"
          [ Connection [1] [2] "" ] [1]
       where
         f = InnerMostState  1 "State 2a" ""
         g = InnerMostState  2 "State 2b" ""
      e = History 3 Deep
   b = InnerMostState  2 "State 3" ""

forCheckSDNameUniq2 :: UMLStateDiagram String Int
forCheckSDNameUniq2
  = umlStateDiagram $
    StateDiagram [a] 1 "A" [] []
     where
      a = StateDiagram [ InnerMostState  1 "A" "" ] 1 "" [] [1]

forCheckSubNameUniq2 :: UMLStateDiagram String Int
forCheckSubNameUniq2
  = umlStateDiagram $ StateDiagram [a,b] 1 ""
    [ Connection[1] [2] "" ] [1]
  where
   a = InnerMostState 1 "A" ""
   b = InnerMostState  2 "A" ""

forCheckUniqueness1 :: UMLStateDiagram String Int
forCheckUniqueness1
  = umlStateDiagram $
    StateDiagram [a, b] 1 "" [] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 1 "B" ""


forCheckUniqueness2 :: UMLStateDiagram String Int
forCheckUniqueness2
  = umlStateDiagram $
    StateDiagram [a, b, c, d, l] 1 "" [] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, i] 1 "" [] [2]
          where
            g = InnerMostState 2 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
        f = StateDiagram [j, k] 2 "" [] [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    c = Fork 3 -- not connected, let it be a Fork
    d = InnerMostState 4 "G" ""
    l = EndState 5

forCheckUniqueness3 :: UMLStateDiagram String Int
forCheckUniqueness3
  = umlStateDiagram $
    StateDiagram [a, b, c, d] 1 ""
    [ Connection [1] [4] "a"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 1
      where
        e = StateDiagram [g, h, i] 1 ""
            [ Connection [1] [2] "b"
            , Connection [2] [3] "c"] [1]
          where
            g = InnerMostState 1 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
        f = StateDiagram [j, k] 2 ""
            [ Connection [1] [2] "e" ] [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    c = Fork 3 -- is that thing even connected to anything?
    d = InnerMostState 4 "G" ""

forCheckUniqueness4 :: UMLStateDiagram String Int
forCheckUniqueness4
  = umlStateDiagram $ StateDiagram [a,b] 1 "" [] []
  where
  a = StateDiagram  [c,d,e] 1 "Composite State" [] [1]
     where
       c = InnerMostState 1 "State 1" ""
       d = StateDiagram [f,g] 2 "state 2" [] [1]
         where
           f = InnerMostState 1 "State 2a" ""
           g = InnerMostState 2 "State 2b" ""
       e = History 2 Deep
  b = InnerMostState 2 "State 3" ""

forCheckUniqueness5 ::UMLStateDiagram String Int
forCheckUniqueness5
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection [1] [2] "t"
    , Connection [2] [1,3] ""] []
  where
    a = StateDiagram  [c,d,e] 1 "Composite State"
        [ Connection [1] [2] "" ] [1]
      where
        c = InnerMostState  1 "State 1" ""
        d = StateDiagram  [f,g] 2 "state 2" [] [1]
          where
            f = InnerMostState 1 "State 2a" ""
            g = InnerMostState 1 "State 2b" "" -- changed this line
        e = History 3 Deep
    b = InnerMostState 2 "State 3" ""

forCheckUniqueness6::UMLStateDiagram String Int
forCheckUniqueness6
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection [1] [2] "t"
    , Connection [2] [1,3] "" ] []
  where
    a = StateDiagram  [c,d,e] 1 "Composite State" [] [1]
      where
        c = InnerMostState  1 "State 1" ""
        d = StateDiagram  [f,g] 1 "state 2"
            [ Connection [1] [2] "" ] [1]
         where
           f = InnerMostState  1 "State 2a" ""
           g = InnerMostState  2 "State 2b" ""
        e = History 3 Deep
    b = InnerMostState  2 "State 3" ""

forCheckUniqueness7 :: UMLStateDiagram String Int
forCheckUniqueness7
  = umlStateDiagram $
    StateDiagram [ CombineDiagram [a,b,c] 1]  1 "active" [] [1]
  where
    a = StateDiagram  [d,e] 1 ""
        [ Connection [1] [2] "EvNumLockPressed"
        , Connection [2] [1] "EvNumLockPressed" ] [1]
      where d = InnerMostState 1 "NumLockOff" ""
            e = InnerMostState 2 "NumLockOn" ""
    b = StateDiagram  [f,g] 1 ""
        [ Connection [1] [2] "EvCapsLockPressed"
        , Connection [2] [1] "EvCapsLockPressed"] [1] -- changed this line
      where f = InnerMostState 1 "CapsLockOff" ""
            g = InnerMostState 2 "CapsLockOn" ""
    c = StateDiagram  [h,i] 1 ""
        [ Connection [1] [2] "EvScrollLockPressed"
        , Connection [2] [1] "EvScrollLockPressed"] [1] -- changed this line
      where h = InnerMostState 1 "ScrollLockOff" ""
            i = InnerMostState 2 "ScrollLockOn" ""

forCheckSubstateSD1::UMLStateDiagram String Int
forCheckSubstateSD1
  = umlStateDiagram $ StateDiagram [] 1 "" [] []

forCheckSubstateSD2::UMLStateDiagram String Int
forCheckSubstateSD2
  = umlStateDiagram $
    StateDiagram [a] 1 "" [] []
  where
    a = CombineDiagram [c,d] 1
      where
        c = StateDiagram [ History 1 Deep ] 1 "" [] [1]
        d = StateDiagram [ InnerMostState 1 "A" "" ] 2 "" [] [1]

forCheckSubstateSD3::UMLStateDiagram String Int
forCheckSubstateSD3
  = umlStateDiagram $ StateDiagram [a] 1 "" [] [1]
    where
      a = History 1 Deep

forCheckSubstateCD1 :: UMLStateDiagram String Int
forCheckSubstateCD1
  = umlStateDiagram $
    StateDiagram [ CombineDiagram [a] 1 ] 1 "" [] []
  where
    a = StateDiagram  [ InnerMostState 1 "A" "" ] 1 ""  [] [1]

forCheckSubstateCD2 :: UMLStateDiagram String Int
forCheckSubstateCD2
  = umlStateDiagram $
    StateDiagram [ CombineDiagram [a,b] 1 ] 1 "" [] [1,2]
  where
    a = StateDiagram  [ InnerMostState 1 "A" "" ] 1 "" [] [1]
    b = InnerMostState 2 "" ""

forCheckHistOutTransition1 :: UMLStateDiagram String Int
forCheckHistOutTransition1
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection [1,3] [1,2,2] "error" ] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

forCheckHistOutTransition2 :: UMLStateDiagram String Int
forCheckHistOutTransition2
  = umlStateDiagram $
    StateDiagram [ Fork 1, a] 1 ""
    [ Connection [1] [2, 1, 1, 1, 1] ""
    , Connection [1] [2, 1, 1, 2, 1] "" ] [1]
  where
    a = StateDiagram [b, c] 2 "A"
        [ Connection [1, 1, 1, 1] [2] "i"
        , Connection [1, 3] [2] ""
        , Connection [1, 2, 2] [2, 2] "h"
        , Connection [2, 2] [1, 1, 2, 3] "e"
        , Connection [1,0] [1,1] "f" ] []
      where
        b = StateDiagram [g, h, i, j] 1 "B"
            [ Connection [1, 1, 2] [3] "c"
            , Connection [1, 2, 2] [3] "c"
            , Connection [0] [2] "a" ] []
          where
            g = CombineDiagram [k, l] 1
              where
                k = StateDiagram
                    [ InnerMostState 1 "1" ""
                    , InnerMostState 2 "2" "" ] 1 "C"
                    [ Connection [1] [2] "a"
                    , Connection [2] [1] "b"] [1]
                l = StateDiagram
                    [ InnerMostState 1 "3" ""
                    , InnerMostState 2 "4" ""
                    , InnerMostState 3 "5" "" ] 2 ""
                    [ Connection [1] [2] "b"
                    , Connection [2] [3] "b"
                    , Connection [3] [1] "b" ] [1]
            h = StateDiagram [m, n] 2 "D"
                [ Connection [1] [2] "g"
                , Connection [2] [1] "g" ] [1]
              where
                m = InnerMostState 1 "9" ""
                n = InnerMostState 2 "10" ""
            i = Join 3
            j = History 0 Shallow
        c = StateDiagram [d, e, f] 2 "E"
            [ Connection [1] [3] "d"
            , Connection [3] [2] "d"
            , Connection [2] [1] "d" ] [3]
          where
            d = InnerMostState 1 "6" ""
            e = InnerMostState 2 "7" ""
            f = InnerMostState 3 "8" ""

forCheckHistOutTransition3 :: UMLStateDiagram String Int
forCheckHistOutTransition3
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection [1] [2] "error" ] [1]
  where
    a = History 1 Deep
    b = InnerMostState  2 "A" ""

forCheckInEdge1 :: UMLStateDiagram String Int
forCheckInEdge1
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection [1] [2] "t"
    , Connection [2] [1,3] "" ] []
  where
   a = StateDiagram  [c,d,e] 1 "Composite State"
       [ Connection [1] [2] ""
       , Connection [2,1] [3] "a" ] [1]
     where
       c = InnerMostState  1 "State 1" ""
       d = StateDiagram [f,g] 2 "state 2"
           [ Connection [1] [2] "b" ] [1]
         where
           f = InnerMostState  1 "State 2a" ""
           g = InnerMostState  2 "State 2b" ""
       e = History 3 Deep
   b = InnerMostState  2 "State 3" ""

forCheckInEdge3 :: UMLStateDiagram String Int
forCheckInEdge3
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection [2] [1] "" ] [2]
  where
    a =  History 1 Deep
    b = InnerMostState  2 "A" ""

forCheckInEdge2 :: UMLStateDiagram String Int
forCheckInEdge2
  = umlStateDiagram $ StateDiagram [a,b] 1 "" [] [2]
  where
    a = StateDiagram  [c,d,e] 1 "A"
        [ Connection [1] [3] "" ] [1]
      where
        c = InnerMostState  1 "B" ""
        d = StateDiagram  [f,g] 2 "C"
            [ Connection [1] [2] "b" ] [1]
          where
            f = InnerMostState  1 "D" ""
            g = InnerMostState  2 "E" ""
        e = History 3 Deep
    b = InnerMostState  2 "State 3" ""

forCheckOutEdge1 :: UMLStateDiagram String Int
forCheckOutEdge1
  = umlStateDiagram $
    StateDiagram [a,b] 1 ""
    [ Connection[1,3] [2] ""
    , Connection[2] [1,3] "" ] []
  where
    a = StateDiagram  [c,d,e] 1 "Composite State" [] [1]
      where
        c = InnerMostState  1 "State 1" ""
        d = StateDiagram  [f,g] 2 "state 2"
            [ Connection [1] [2] "b" ] [1]
          where
            f = InnerMostState  1 "State 2a" ""
            g = InnerMostState  2 "State 2b" ""
        e = History 3 Deep
    b = InnerMostState  2 "State 3" ""

forCheckOutEdge2 :: UMLStateDiagram String Int
forCheckOutEdge2
  = umlStateDiagram $ StateDiagram [a,b] 1 ""
    [ Connection [1,3] [2] "" ] [1,3]
  where
    a = StateDiagram  [c,d] 1 "" [] [1]
      where
        c = InnerMostState 1 "A" ""
        d = History 3 Deep
    b = InnerMostState  2 "A" ""

forCheckEmptyConnPoint1 :: UMLStateDiagram String Int
forCheckEmptyConnPoint1
  = umlStateDiagram $
    StateDiagram [a,b] 1 "" [ Connection[] [1] "a" ] [2]
  where a = InnerMostState  1 "A" ""
        b = InnerMostState  2 "B" ""

forCheckEmptyConnPoint2 :: UMLStateDiagram String Int
forCheckEmptyConnPoint2
  = umlStateDiagram $
    StateDiagram [a, b, c] 1 ""
    [ Connection [1] [3] "k"
    , Connection [2] [3] "k"
    , Connection [3, 2] [2] "h"
    , Connection [2] [1, 2, 2] "h" ] [3]
  where
    a = CombineDiagram [d, e] 1
      where
        d = StateDiagram [h, i] 1 ""
            [ Connection [1] [2] "a"
            , Connection [2] [1] "a"] [1]
          where
            h = InnerMostState 1 "A" ""
            i = InnerMostState 2 "B" ""
        e = StateDiagram [j, k] 2 ""
            [ Connection [1] [] "b"
            , Connection [2] [1] "b"] [1]
          where
            j = InnerMostState 1 "C" ""
            k = InnerMostState 2 "D" ""
    b = InnerMostState 2 "E" ""
    c = StateDiagram [f, g] 3 ""
        [ Connection [1] [2] "a"
        , Connection [2] [1] "a"] [1]
      where
        f = InnerMostState 1 "F" ""
        g = InnerMostState 2 "G" ""

forCheckSameConnection1 :: UMLStateDiagram String Int
forCheckSameConnection1
  = umlStateDiagram $
    StateDiagram [a, b, c, d] 1 ""
    [ Connection [1] [2] "a"
    , Connection [2] [1] "f"
    , Connection [2] [3, 0] "b"
    , Connection [3] [1] "e"
    , Connection [4] [3] "x"] [4]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""
    c = StateDiagram [e, f, g] 3 ""
        [ Connection [1] [2] "c"
        , Connection [2] [1] ""
        , Connection [2] [0] ""] [1]
      where
        e = InnerMostState 1 "C" ""
        f = InnerMostState 2 "D" ""
        g = InnerMostState 0 "E" ""
    d = InnerMostState 4 "X" ""

forCheckSameConnection2 :: UMLStateDiagram String Int
forCheckSameConnection2
  = umlStateDiagram $
    StateDiagram [a, b, c, d, l] 1 ""
    [ Connection [1] [2] "a"
    , Connection [2, 1, 2] [4] "h"
    , Connection [2, 1, 3] [3] ""
    , Connection [2, 2, 2] [3] ""
    , Connection [3] [4] "g"
    ,Connection [4] [5] "" ] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, i] 1 ""
            [ Connection [1] [2] "b"
            , Connection [1] [3] "b" ] [1]
          where
            g = InnerMostState 1 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
        f = StateDiagram [j, k] 2 ""
            [ Connection [1] [2] "e" ] [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    c = Join 3
    d = InnerMostState 4 "G" ""
    l = EndState 5

forCheckSameConnection3 :: UMLStateDiagram String Int
forCheckSameConnection3
  = umlStateDiagram $
    StateDiagram [a, b] 1 ""
    [ Connection [1] [2] "a"
    , Connection [1] [2] "a"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""

forCheckEmptyTran1 :: UMLStateDiagram String Int
forCheckEmptyTran1
  = umlStateDiagram $
    StateDiagram [a,b,c,d,e,f] 1 "order of management system"
    [ Connection [1] [2] ""
    , Connection [2] [3] "Action"
    , Connection [3] [4] "Confirm order(Event)"
    , Connection [4] [5] ""
    , Connection [5] [6] ""
    , Connection [5] [6] "complete" ] [1]
  where
    a = InnerMostState 1 "idle" ""
    b = InnerMostState 2 "Send order request" ""
    c = InnerMostState 3 "Select normal or special order" ""
    d = InnerMostState 4 "Order confirmation" ""
    e = InnerMostState 5 "Dispatch order" ""
    f = EndState 6

forCheckEmptyTran2 :: UMLStateDiagram String Int
forCheckEmptyTran2
  = umlStateDiagram $
    StateDiagram [a, b, c] 1 ""
    [ Connection [1] [3] "k"
    , Connection [2] [3] "k"
    , Connection [3, 2] [2] "h"
    , Connection [2] [1, 2, 2] "h" ] [3]
  where
    a = CombineDiagram [d, e] 1
      where
        d = StateDiagram [h, i] 1 ""
            [ Connection [1] [2] "a"
            , Connection [2] [1] "a"] [1]
          where
            h = InnerMostState 1 "A" ""
            i = InnerMostState 2 "B" ""
        e = StateDiagram [j, k,m] 2 ""
            [ Connection [1] [2] "b"
            , Connection [1] [3] ""] [1]
          where
            j = InnerMostState 1 "C" ""
            k = InnerMostState 2 "D" ""
            m = InnerMostState 3 "H" ""
    b = InnerMostState 2 "E" ""
    c = StateDiagram [f, g] 3 ""
        [ Connection [1] [2] "a"
        , Connection [2] [1] "a"] [1]
      where
        f = InnerMostState 1 "F" ""
        g = InnerMostState 2 "G" ""

forCheckEmptyTran3 :: UMLStateDiagram String Int
forCheckEmptyTran3
  = umlStateDiagram $
    StateDiagram [a, b] 1 ""
    [ Connection [1] [2] "a"
    , Connection [1] [2] "" ] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""

{- should really be rewritten into partial composition instead of using the debug show output -}
forCheckDrawability :: UMLStateDiagram String Int
forCheckDrawability
  = umlStateDiagram $
    StateDiagram
    {
      substates
        = [ StateDiagram
            {
              substates
                = [ CombineDiagram
                    {
                      substates
                        = [ StateDiagram
                            {
                              substates = [ InnerMostState { label = 1
                                                           , name = "1.1"
                                                           , operations = ""
                                                           }
                                          ]
                            , label = 1
                            , name = "1"
                            , connections = []
                            , startState = []
                            }
                          , StateDiagram
                            {
                              substates
                                = [ StateDiagram
                                    {
                                      substates = [ InnerMostState { label = 5
                                                                   , name = "Q"
                                                                   , operations = ""
                                                                   }
                                                  , InnerMostState { label = 2
                                                                   , name = "R"
                                                                   , operations = ""
                                                                   }
                                                  , EndState { label = 1 }
                                                  ]
                                    , label = 2
                                    , name = "E"
                                    , connections = []
                                    , startState = [1]
                                    }
                                    , InnerMostState { label = 1
                                                     , name = "J"
                                                     , operations = ""
                                                     }
                                    , StateDiagram
                                      {
                                        substates
                                          = [ InnerMostState { label = 3
                                                             , name = "W"
                                                             , operations = ""
                                                             }
                                            ]
                                      , label = 4
                                      , name = "M"
                                      , connections = []
                                      , startState = []
                                      }
                                  ]
                            , label = 2
                            , name = "N"
                            , connections = [ Connection { pointFrom = [2,2]
                                                         , pointTo = [ 2 ]
                                                         , transition = "k"
                                                         }
                                            ]
                            , startState = [4,3]
                            }
                          ]
                    , label = 2
                    }
                    , InnerMostState { label = 3
                                     , name = "F"
                                     , operations = ""
                                     }
                ]
            , label = 2
            , name = "B"
            , connections = []
            , startState = []
            }
          ]
    , label = 1
    , name = "I"
    , connections
        = [ Connection { pointFrom = [2,2,2,4]
                       , pointTo = [2,2,2,1]
                       , transition = "a"
                      }
          , Connection { pointFrom = [2,2]
                       , pointTo = [2,2,2,2,5]
                       , transition = "c"
                       }
          ]
    , startState = []
    }
