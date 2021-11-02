module CounterExample where
import Datatype (
  Connection(..),
  HistoryType(..),
  StateDiagram(..),
  UMLStateDiagram,
  )

{-# ANN module "Hlint: ignore Reduce duplication" #-}

forCheckReachablity1 :: UMLStateDiagram
forCheckReachablity1 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2] "a", Connection [1] [3] "b",
        Connection [3] [2] "c", Connection [4] [3] "d", Connection [3] [1] "e"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""
    c = InnerMostState 3 "C" ""
    d = InnerMostState 4 "D" ""

forCheckReachablity2 :: UMLStateDiagram
forCheckReachablity2 = StateDiagram [a, b, c, d] 1 "" [Connection [1, 1, 3] [2] "e",
          Connection [1, 2, 2] [2] "e", Connection [2] [3] "", Connection [3]
          [4] "c", Connection [3] [4, 2] "d", Connection [4, 2] [1, 2, 2] "a",
          Connection [4, 1] [1] "a"] [1, 1, 2]
  where
    a = CombineDiagram [e, f] 1
      where
        e = StateDiagram [i, j, k] 1 "" [Connection [1] [2] "a", Connection
            [2] [3] "b", Connection [3] [1] "a"] [1]
          where
            i = InnerMostState 1 "A" ""
            j = InnerMostState 2 "B" ""
            k = InnerMostState 3 "C" ""
        f = StateDiagram [l, m] 2 "" [Connection [1] [2] "b", Connection [2]
            [1] "c"] [1]
          where
            l = InnerMostState 1 "D" ""
            m = InnerMostState 2 "E" ""
    b = Joint 2
    c = InnerMostState 3 "F" ""
    d = StateDiagram [g, h] 4 "" [] [2]
      where
        g = InnerMostState 1 "G" ""
        h = InnerMostState 2 "H" ""

bogusExample :: UMLStateDiagram
bogusExample = StateDiagram [CombineDiagram [a,b] 1] 0 "" [Connection [1,2,0] [1,3,0] ""] []
    where
      a = StateDiagram [InnerMostState 0 "" ""] 2 "" [] []
      b = StateDiagram [InnerMostState 0 "" ""] 3 "" [] []

forCheckTransition1 :: UMLStateDiagram
forCheckTransition1 = StateDiagram [a,b,c,d,e,f,g] 1 "" [Connection[5] [1] "a",Connection[1] [2] "a",Connection[2] [3] "",
      Connection[3] [6] "",Connection[5] [4] "",Connection[4] [6] "",Connection[6] [7] ""] [5]
     where a = InnerMostState  1 "Tasse nehmen" ""
           b = InnerMostState  2 "Kaffee trinken" ""
           c = InnerMostState  3 "Tasse absetzen" ""
           d = InnerMostState  4 "Zeitung lesen" ""
           e = Joint 5
           f = Joint 6        
           g = EndState 7

forCheckTransition2 :: UMLStateDiagram
forCheckTransition2 = StateDiagram [a,b,c,d,e,f,g] 1 "" [Connection[5] [1] "a",Connection[1] [2] "a",Connection[2] [3] "",
      Connection[3] [6] "a",Connection[5] [4] "a",Connection[4] [6] "",Connection[6] [7] ""] [5]
     where a = InnerMostState  1 "Tasse nehmen" ""
           b = InnerMostState  2 "Kaffee trinken" ""
           c = InnerMostState  3 "Tasse absetzen" ""
           d = InnerMostState  4 "Zeitung lesen" ""
           e = Joint 5
           f = Joint 6
           g = EndState 7

forCheckMtoOne2 :: UMLStateDiagram
forCheckMtoOne2 =  StateDiagram [a,b,c,d,e] 1 "" [Connection[1] [5] "",Connection[5] [4] ""] [5]
     where a = InnerMostState  1 "A" ""
           b = InnerMostState  2 "B" ""
           c = InnerMostState  3 "C" ""
           d = InnerMostState  4 "D" ""
           e = Joint 5

forCheckMtoOne1 :: UMLStateDiagram
forCheckMtoOne1 =  StateDiagram [a,b,c,d,e] 1 "" [Connection[2] [5] "",Connection[1] [5] "",Connection[5] [4] "a",
      Connection[5] [3] "a"] [2]
     where a = InnerMostState  1 "A" ""
           b = InnerMostState  2 "B" ""
           c = InnerMostState  3 "C" ""
           d = InnerMostState  4 "D" ""
           e = Joint 5

forCheckMtoOne3 :: UMLStateDiagram
forCheckMtoOne3 =  StateDiagram [a,b,c,d,e] 1 "" [Connection[5] [4] ""] [5]
     where a = InnerMostState  1 "A" ""
           b = InnerMostState  2 "B" ""
           c = InnerMostState  3 "C" ""
           d = InnerMostState  4 "D" ""
           e = Joint 5

forCheckMtoOne4 :: UMLStateDiagram
forCheckMtoOne4 =  StateDiagram [a,b,c,d,e] 1 "" [Connection[5] [4] "",Connection[5] [3] ""] []
     where a = InnerMostState  1 "A" ""
           b = InnerMostState  2 "B" ""
           c = InnerMostState  3 "C" ""
           d = InnerMostState  4 "D" ""
           e = Joint 5

forCheckMtoOne5 :: UMLStateDiagram
forCheckMtoOne5 =  StateDiagram [a,b,c,d,e] 1 "" [Connection[4] [5] "",Connection[3] [5] ""] []
     where a = InnerMostState  1 "A" ""
           b = InnerMostState  2 "B" ""
           c = InnerMostState  3 "C" ""
           d = InnerMostState  4 "D" ""
           e = Joint 5

forCheckMtoOne6 :: UMLStateDiagram
forCheckMtoOne6 =  StateDiagram [a,b,c,d,e] 1 "" [Connection[4] [5] "",Connection[5] [2] ""] []
     where a = InnerMostState  1 "A" ""
           b = InnerMostState  2 "B" ""
           c = InnerMostState  3 "C" ""
           d = InnerMostState  4 "D" ""
           e = Joint 5

forCheckMtoOne7 :: UMLStateDiagram
forCheckMtoOne7 = StateDiagram [a, b, c, d, l] 1 "" [ Connection [2, 1, 3] [3] "", Connection [2, 2, 2]
           [3] "", Connection [3] [4] "g",Connection [3] [2] "g"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, i] 1 "" [] [1]
          where
            g = InnerMostState 1 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
        f = StateDiagram [j, k] 2 "" [] [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    c = Joint 3
    d = InnerMostState 4 "G" ""
    l = EndState 5

forCheckMtoOne8 :: UMLStateDiagram
forCheckMtoOne8 = StateDiagram [a,b,c,d,e] 1 "" [] [5]
     where a = InnerMostState  1 "A" ""
           b = InnerMostState  2 "B" ""
           c = InnerMostState  3 "C" ""
           d = InnerMostState  4 "D" ""
           e = Joint 5

forCheckTransition3 :: UMLStateDiagram
forCheckTransition3 = StateDiagram [a, b, d, l] 1 "" [Connection [2]
           [2, 1, 4] "a", Connection [4] [2, 1, 4] "b"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, c, i] 1 "" [Connection [4] [2] "c"] [1]
          where
            g = InnerMostState 1 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
            c = Joint 4
        f = StateDiagram [j, k] 2 "" [] [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    d = InnerMostState 4 "G" ""
    l = EndState 5

forCheckTransition4 :: UMLStateDiagram
forCheckTransition4 = StateDiagram [a,b,c,d,e] 1 "" [Connection[5] [4] "a",Connection[5] [2] "c"] [5]
     where a = InnerMostState  1 "A" ""
           b = InnerMostState  2 "B" ""
           c = InnerMostState  3 "C" ""
           d = InnerMostState  4 "D" ""
           e = Joint 5

forCheckEndState1 :: UMLStateDiagram
forCheckEndState1 = StateDiagram [a,b,c,d,e] 1 "" [Connection[7] [2] ""] [2]
     where a = InnerMostState  1 "Tasse nehmen" ""
           b = InnerMostState  2 "Kaffee trinken" ""
           c = InnerMostState  3 "Tasse absetzen" ""
           d = InnerMostState  4 "Zeitung lesen" ""
           e = EndState 7

forCheckEndState2 :: UMLStateDiagram
forCheckEndState2 = StateDiagram [a, b] 1 "" [Connection [1,3] [2] "a"] []
  where
    a = StateDiagram [c, d, e] 1 "A" [Connection [1] [2] "", Connection [2]
        [3] "b", Connection [3] [2] "b", Connection [2] [1] "a"] [1]
      where
        c = InnerMostState 1 "B" ""
        d = InnerMostState 2 "C" ""
        e = EndState 3
    b = InnerMostState 2 "E" ""


forCheckSubS1 :: UMLStateDiagram
forCheckSubS1 = StateDiagram [InnerMostState 1 "" "", InnerMostState 2 "" ""] 1 "" [] [1,2]

forCheckSubS2 :: UMLStateDiagram
forCheckSubS2 = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [1,5,2]  
  where
   a = StateDiagram  [d,e] 1 ""  [Connection [1] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
     where d = InnerMostState 1 "NumLockOff" ""
           e = InnerMostState 2 "NumLockOn" ""
   b = StateDiagram  [f,g] 2 ""  [Connection [1] [2] "EvCapsLockPressed", Connection [2] [1] "EvCapsLockPressed"] [1]
     where f = InnerMostState 1 "CapsLockOff" ""
           g = InnerMostState 2 "CapsLockOn" ""
   c = StateDiagram  [h,i] 3 ""  [Connection [1] [2] "EvScrollLockPressed", Connection [2] [1] "EvScrollLockPressed"] [1]
     where h = InnerMostState 1 "ScrollLockOff" ""
           i = InnerMostState 2 "ScrollLockOn" ""

forCheckSubS3 :: UMLStateDiagram
forCheckSubS3 = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [5]
  where
   a = StateDiagram  [d,e] 1 ""  [Connection [1] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
     where d = InnerMostState 1 "NumLockOff" ""
           e = InnerMostState 2 "NumLockOn" ""
   b = StateDiagram  [f,g] 2 ""  [Connection [1] [2] "EvCapsLockPressed", Connection [2] [1] "EvCapsLockPressed"] [1]
    where f = InnerMostState 1 "CapsLockOff" ""
          g = InnerMostState 2 "CapsLockOn" ""
   c = StateDiagram  [h,i] 3 ""  [Connection [1] [2] "EvScrollLockPressed", Connection [2] [1] "EvScrollLockPressed"] [1]
    where h = InnerMostState 1 "ScrollLockOff" ""
          i = InnerMostState 2 "ScrollLockOn" ""

forCheckSubS4 :: UMLStateDiagram
forCheckSubS4 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [5]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [2] ""] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

forCheckSubS5 :: UMLStateDiagram
forCheckSubS5 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [2,1,3]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [2] ""] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

forCheckSubS6 :: UMLStateDiagram
forCheckSubS6 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2] "a", Connection
           [2, 1, 2] [4] "h", Connection [2, 1, 3] [3] "", Connection [2, 2, 2]
           [3] "", Connection [3] [4] "g"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, i] 1 "" [Connection [1] [2] "b", Connection
            [2] [3] "c"] [5]
          where
            g = InnerMostState 1 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
        f = StateDiagram [j, k] 2 "" [Connection [1] [2] "e"] [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    c = Joint 3
    d = InnerMostState 4 "G" ""

forCheckStartToRegion1 ::UMLStateDiagram
forCheckStartToRegion1 = StateDiagram [CombineDiagram [a,b] 1, InnerMostState  2 "" ""] 0 "" [Connection [1,3,0] [2] ""] [1,3]
    where
      a = StateDiagram [InnerMostState 0 "" ""] 3 "" [] []
      b = StateDiagram [InnerMostState 0 "" ""] 4 "" [] []

forCheckStartToRegion2 ::UMLStateDiagram
forCheckStartToRegion2 = StateDiagram [CombineDiagram [a,b] 1, InnerMostState  2 "" ""] 0 "" [Connection [1] [2] ""] []
    where
      a = StateDiagram [c,d] 3 "" [] [1,1]
        where 
          c = CombineDiagram [e,f] 1
            where 
                e = StateDiagram [InnerMostState 0 "" ""] 1 "" [] []
                f = StateDiagram [InnerMostState 0 "" ""] 2 "" [] []
          d = InnerMostState  2 "" ""
      b = StateDiagram [InnerMostState 0 "" ""] 4 "" [] []
      
forCheckConnection1 :: UMLStateDiagram
forCheckConnection1 = StateDiagram [a, b, c, d] 1 "" [Connection [5] [3] "a", Connection
           [1] [2] "f", Connection [2] [4] "b", Connection [3] [4] "c",
           Connection [3] [1] "e", Connection [4] [3] "d", Connection [4] [1]
           "e"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""
    c = InnerMostState 3 "C" ""
    d = InnerMostState 4 "D" ""

forCheckConnection2 :: UMLStateDiagram
forCheckConnection2 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2] "a", Connection
               [2, 1, 2] [4] "h", Connection [2, 1, 4] [3] "", Connection [2, 2, 2]
               [3] "", Connection [3] [4] "g"] [1]
      where
        a = InnerMostState 1 "A" ""
        b = CombineDiagram [e, f] 2
          where
            e = StateDiagram [g, h, i] 1 "" [Connection [1] [2] "b", Connection
                [2] [3] "c"] [1]
              where
                g = InnerMostState 1 "B" ""
                h = InnerMostState 2 "C" ""
                i = InnerMostState 3 "D" ""
            f = StateDiagram [j, k] 2 "" [Connection [1] [2] "e"] [1]
              where
                j = InnerMostState 1 "E" ""
                k = InnerMostState 2 "F" ""
        c = Joint 3
        d = InnerMostState 4 "G" ""

forCheckConnection3 :: UMLStateDiagram
forCheckConnection3 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2] "a", Connection
                       [2, 1, 2] [4] "h", Connection [2, 4, 3] [3] "", Connection [2, 2, 2]
                       [3] "", Connection [3] [4] "g"] [1]
        where
          a = InnerMostState 1 "A" ""
          b = CombineDiagram [e, f] 2
            where
              e = StateDiagram [g, h, i] 1 "" [Connection [1] [2] "b", Connection
                  [2] [3] "c"] [1]
                where
                  g = InnerMostState 1 "B" ""
                  h = InnerMostState 2 "C" ""
                  i = InnerMostState 3 "D" ""
              f = StateDiagram [j, k] 2 "" [Connection [1] [2] "e"] [1]
                where
                  j = InnerMostState 1 "E" ""
                  k = InnerMostState 2 "F" ""
          c = Joint 3
          d = InnerMostState 4 "G" ""

forCheckConnection4 :: UMLStateDiagram
forCheckConnection4 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
      where
       a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [5] [2] ""] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [2] ""] [1]
            where
             f = InnerMostState  1 "State 2a" ""
             g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
       b = InnerMostState  2 "State 3" ""

forCheckConnection5::UMLStateDiagram
forCheckConnection5 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [99] ""] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

forCheckConnection6::UMLStateDiagram
forCheckConnection6 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [0] ""] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""


forCheckConnection7 :: UMLStateDiagram
forCheckConnection7 = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [1,2,2]
  where
   a = StateDiagram  [d,e] 1 ""  [Connection [4] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
     where d = InnerMostState 1 "NumLockOff" ""
           e = InnerMostState 2 "NumLockOn" ""
   b = StateDiagram  [f,g] 2 ""  [Connection [1] [2] "EvCapsLockPressed", Connection [2] [1] "EvCapsLockPressed"] [1]
    where f = InnerMostState 1 "CapsLockOff" ""
          g = InnerMostState 2 "CapsLockOn" ""
   c = StateDiagram  [h,i] 3 ""  [Connection [1] [2] "EvScrollLockPressed", Connection [2] [1] "EvScrollLockPressed"] [1]
    where h = InnerMostState 1 "ScrollLockOff" ""
          i = InnerMostState 2 "ScrollLockOn" ""

forCheckConnection8 :: UMLStateDiagram
forCheckConnection8 = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [1,2,2]
  where
   a = StateDiagram  [d,e] 1 ""  [Connection [2,3] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
     where d = InnerMostState 1 "NumLockOff" ""
           e = InnerMostState 2 "NumLockOn" ""
   b = StateDiagram  [f,g] 2 ""  [Connection [1] [2] "EvCapsLockPressed", Connection [2] [1] "EvCapsLockPressed"] [1]
     where f = InnerMostState 1 "CapsLockOff" ""
           g = InnerMostState 2 "CapsLockOn" ""
   c = StateDiagram  [h,i] 3 ""  [Connection [1] [2] "EvScrollLockPressed", Connection [2] [1] "EvScrollLockPressed"] [1]
     where h = InnerMostState 1 "ScrollLockOff" ""
           i = InnerMostState 2 "ScrollLockOn" ""

forCheckConnFromToRegion1 ::UMLStateDiagram
forCheckConnFromToRegion1 = StateDiagram [CombineDiagram [a,b] 1, InnerMostState  2 "" ""] 0 "" [Connection [1,3] [2] ""] []
    where
      a = StateDiagram [InnerMostState 0 "" ""] 3 "" [] []
      b = StateDiagram [InnerMostState 0 "" ""] 4 "" [] []

forCheckConnFromToRegion2 ::UMLStateDiagram
forCheckConnFromToRegion2 = StateDiagram [CombineDiagram [a,b] 1, InnerMostState  2 "" ""] 0 "" [Connection [2] [1,3] ""] []
    where
      a = StateDiagram [InnerMostState 0 "" ""] 3 "" [] []
      b = StateDiagram [InnerMostState 0 "" ""] 4 "" [] []

forCheckConnFromToRegion3 ::UMLStateDiagram
forCheckConnFromToRegion3 = StateDiagram [CombineDiagram [a,b] 1, InnerMostState  2 "" ""] 0 "" [] []
    where
      a = StateDiagram [c,d] 3 "" [Connection [1,1] [2] ""] []
        where 
          c = CombineDiagram [e,f] 1
            where 
                e = StateDiagram [InnerMostState 0 "" ""] 1 "" [] []
                f = StateDiagram [InnerMostState 0 "" ""] 2 "" [] []
          d = InnerMostState  2 "" ""
      b = StateDiagram [InnerMostState 0 "" ""] 4 "" [] []

forCheckConnFromToRegion4 ::UMLStateDiagram
forCheckConnFromToRegion4 = StateDiagram [CombineDiagram [a,b] 1, InnerMostState  2 "" ""] 0 "" [Connection [1,3,1,1] [2] ""] []
    where
      a = StateDiagram [c,d] 3 "" [] []
        where 
          c = CombineDiagram [e,f] 1
            where 
                e = StateDiagram [InnerMostState 0 "" ""] 1 "" [] []
                f = StateDiagram [InnerMostState 0 "" ""] 2 "" [] []
          d = InnerMostState  2 "" ""
      b = StateDiagram [InnerMostState 0 "" ""] 4 "" [] []


forCheckNameUniqueness1 :: UMLStateDiagram
forCheckNameUniqueness1 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
          where
           c = InnerMostState  1 "a" ""
           d = StateDiagram  [f,g] 2 "a" [Connection [1] [2] ""] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

forCheckNameUniqueness2 :: UMLStateDiagram
forCheckNameUniqueness2 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "a" [Connection [1] [2] ""] [1]
          where
           c = InnerMostState  1 "a" ""
           d = StateDiagram  [f,g] 2 "State 2" [Connection [1] [2] ""] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""


forCheckUniqueness1 :: UMLStateDiagram
forCheckUniqueness1 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [3] "a", Connection
           [1] [1] "f", Connection [1] [4] "b", Connection [3] [4] "c",
           Connection [3] [1] "e", Connection [4] [3] "d", Connection [4] [1]
           "e"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 1 "B" "" 
    c = InnerMostState 3 "C" ""
    d = InnerMostState 4 "D" ""

forCheckUniqueness2 :: UMLStateDiagram
forCheckUniqueness2 =  StateDiagram [a, b, c, d, l] 1 "" [] [1]
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
    c = Joint 3
    d = InnerMostState 4 "G" ""
    l = EndState 5

forCheckUniqueness3 :: UMLStateDiagram
forCheckUniqueness3 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [4] "a"] [1]
        where
          a = InnerMostState 1 "A" ""
          b = CombineDiagram [e, f] 1  
            where
              e = StateDiagram [g, h, i] 1 "" [Connection [1] [2] "b", Connection
                  [2] [3] "c"] [1]
                where
                  g = InnerMostState 1 "B" ""
                  h = InnerMostState 2 "C" ""
                  i = InnerMostState 3 "D" ""
              f = StateDiagram [j, k] 2 "" [Connection [1] [2] "e"] [1]
                where
                  j = InnerMostState 1 "E" ""
                  k = InnerMostState 2 "F" ""
          c = Joint 3
          d = InnerMostState 4 "G" ""

forCheckUniqueness4 :: UMLStateDiagram
forCheckUniqueness4 = StateDiagram [a,b] 1 "" [] []
      where
       a = StateDiagram  [c,d,e] 1 "Composite State" [] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [] [1]
            where
             f = InnerMostState  1 "State 2a" ""
             g = InnerMostState  2 "State 2b" ""
           e = History 2 Deep                     
       b = InnerMostState  2 "State 3" ""

forCheckUniqueness5 ::UMLStateDiagram
forCheckUniqueness5 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  1 "State 2b" "" -- changed this line
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

forCheckUniqueness6::UMLStateDiagram
forCheckUniqueness6 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 1 "state 2" [Connection [1] [2] ""] [1] 
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

forCheckUniqueness7 :: UMLStateDiagram
forCheckUniqueness7 = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [1]
  where
    a = StateDiagram  [d,e] 1 ""  [Connection [1] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
      where d = InnerMostState 1 "NumLockOff" ""
            e = InnerMostState 2 "NumLockOn" ""
    b = StateDiagram  [f,g] 1 ""  [Connection [1] [2] "EvCapsLockPressed", Connection [2] [1] "EvCapsLockPressed"] [1] -- changed this line
      where f = InnerMostState 1 "CapsLockOff" ""
            g = InnerMostState 2 "CapsLockOn" ""
    c = StateDiagram  [h,i] 1 ""  [Connection [1] [2] "EvScrollLockPressed", Connection [2] [1] "EvScrollLockPressed"] [1] -- changed this line
      where h = InnerMostState 1 "ScrollLockOff" ""
            i = InnerMostState 2 "ScrollLockOn" ""

forCheckOuterMostLayer :: UMLStateDiagram
forCheckOuterMostLayer = CombineDiagram [a,b,c] 1
  where
   a = StateDiagram  [d,e] 1 ""  [Connection [1] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
     where d = InnerMostState 1 "NumLockOff" ""
           e = InnerMostState 2 "NumLockOn" ""
   b = StateDiagram  [f,g] 2 ""  [Connection [1] [2] "EvCapsLockPressed", Connection [2] [1] "EvCapsLockPressed"] [1]
    where f = InnerMostState 1 "CapsLockOff" ""
          g = InnerMostState 2 "CapsLockOn" ""
   c = StateDiagram  [h,i] 3 ""  [Connection [1] [2] "EvScrollLockPressed", Connection [2] [1] "EvScrollLockPressed"] [1]
    where h = InnerMostState 1 "ScrollLockOff" ""
          i = InnerMostState 2 "ScrollLockOn" ""

forCheckSubstateSD1::UMLStateDiagram
forCheckSubstateSD1 = StateDiagram [a,b] 1 "" [] []
     where
      a = Joint 1
      b = Joint 2

forCheckSubstateSD2::UMLStateDiagram
forCheckSubstateSD2 = StateDiagram [a,b] 1 "" [] []
    where
      a = StateDiagram  [c,d,e] 1 "Composite State" [] [1]
          where
           c = History 1 Deep
           d = Joint 2
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

forCheckSubstateCD1 :: UMLStateDiagram
forCheckSubstateCD1 = StateDiagram [CombineDiagram [a] 1] 1 "active" [] [1]
  where
    a = StateDiagram  [d,e] 1 ""  [Connection [1] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
      where d = InnerMostState 1 "NumLockOff" ""
            e = InnerMostState 2 "NumLockOn" ""

forCheckSubstateCD2 :: UMLStateDiagram
forCheckSubstateCD2 = StateDiagram [CombineDiagram [a,b] 1] 1 "active" [] [1]
  where
    a = StateDiagram  [d,e] 1 ""  [Connection [1] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
      where d = InnerMostState 1 "NumLockOff" ""
            e = InnerMostState 2 "NumLockOn" ""
    b = InnerMostState 2 "" ""

forCheckHistOutTransition1 :: UMLStateDiagram
forCheckHistOutTransition1 = StateDiagram [a,b] 1 "" [Connection[1,3] [1,2,2] "error"] []
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

forCheckHistOutTransition2 :: UMLStateDiagram
forCheckHistOutTransition2 = StateDiagram [Joint 1, a] 1 "" [Connection [1] [2, 1, 1, 1, 1] "",
         Connection [1] [2, 1, 1, 2, 1] ""] [1]
  where
    a = StateDiagram [b, c] 2 "A" [Connection [1, 1, 1, 1] [2] "i",
        Connection [1, 3] [2] "", Connection [1, 2, 2] [2, 2] "h", Connection
        [2, 2] [1, 1, 2, 3] "e", Connection [1,0] [1,1] "f"] []
      where
        b = StateDiagram [g, h, i, j] 1 "B" [Connection [1, 1, 2] [3] "c",
            Connection [1, 2, 2] [3] "c", Connection [0] [2] "a"] []
          where
            g = CombineDiagram [k, l] 1
              where
                k = StateDiagram [InnerMostState 1 "1" "", InnerMostState 2
                    "2" ""] 1 "C" [Connection [1] [2] "a", Connection [2] [1]
                    "b"] [1]
                l = StateDiagram [InnerMostState 1 "3" "", InnerMostState 2
                    "4" "", InnerMostState 3 "5" ""] 2 "" [Connection [1] [2]
                    "b", Connection [2] [3] "b", Connection [3] [1] "b"] [1]
            h = StateDiagram [m, n] 2 "D" [Connection [1] [2] "g", Connection
                [2] [1] "g"] [1]
              where
                m = InnerMostState 1 "9" ""
                n = InnerMostState 2 "10" ""
            i = Joint 3
            j = History 0 Shallow
        c = StateDiagram [d, e, f] 2 "E" [Connection [1] [3] "d", Connection
            [3] [2] "d", Connection [2] [1] "d"] [3]
          where
            d = InnerMostState 1 "6" ""
            e = InnerMostState 2 "7" ""
            f = InnerMostState 3 "8" ""

forCheckInEdge1 :: UMLStateDiagram
forCheckInEdge1 = StateDiagram [a,b] 1 "" [] []
     where
      a = StateDiagram  [c,d,e] 1 "A" [Connection [2,1] [3] ""] [1]
          where
           c = InnerMostState  1 "B" ""
           d = StateDiagram  [f,g] 2 "C" [] [1]
            where
              f = InnerMostState  1 "D" ""
              g = InnerMostState  2 "E" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

forCheckInEdge2 :: UMLStateDiagram
forCheckInEdge2 = StateDiagram [a,b] 1 "" [] []
     where
      a = StateDiagram  [c,d,e] 1 "A" [Connection [1] [3] ""] [1]
          where
           c = InnerMostState  1 "B" ""
           d = StateDiagram  [f,g] 2 "C" [] [1]
            where
              f = InnerMostState  1 "D" ""
              g = InnerMostState  2 "E" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

forCheckOutEdge1 :: UMLStateDiagram
forCheckOutEdge1 = StateDiagram [a,b] 1 "" [Connection[1,3] [2] ""] []
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


forCheckEmptyConnPoint1 :: UMLStateDiagram
forCheckEmptyConnPoint1 = StateDiagram [a,b,c,d,e,f,g] 1 "" [Connection[5] [1] "a",Connection[1] [2] "a",Connection[2] [3] "",
      Connection[] [6] "",Connection[5] [4] "a",Connection[4] [6] "",Connection[6] [7] ""] [5]
     where a = InnerMostState  1 "Tasse nehmen" ""
           b = InnerMostState  2 "Kaffee trinken" ""
           c = InnerMostState  3 "Tasse absetzen" ""
           d = InnerMostState  4 "Zeitung lesen" ""
           e = Joint 5
           f = Joint 6
           g = EndState 7 

forCheckEmptyConnPoint2 :: UMLStateDiagram
forCheckEmptyConnPoint2 =  StateDiagram [a, b, c] 1 "" [Connection [1] [3] "k", Connection [2]
         [3] "k", Connection [3, 2] [2] "h", Connection [2] [1, 2, 2] "h"] [3]
  where
    a = CombineDiagram [d, e] 1
      where
        d = StateDiagram [h, i] 1 "" [Connection [1] [2] "a", Connection [2]
            [1] "a"] [1]
          where
            h = InnerMostState 1 "A" ""
            i = InnerMostState 2 "B" ""
        e = StateDiagram [j, k] 2 "" [Connection [1] [] "b", Connection [2]
            [1] "b"] [1]
          where
            j = InnerMostState 1 "C" ""
            k = InnerMostState 2 "D" ""
    b = InnerMostState 2 "E" ""
    c = StateDiagram [f, g] 3 "" [Connection [1] [2] "a", Connection [2] [1]
        "a"] [1]
      where
        f = InnerMostState 1 "F" ""
        g = InnerMostState 2 "G" ""

forCheckSameConnection1 :: UMLStateDiagram
forCheckSameConnection1 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2] "a", Connection
           [2] [1] "f", Connection [2] [3, 0] "b", Connection [3] [1] "e",
           Connection [4] [3] "x"] [4]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""
    c = StateDiagram [e, f, g] 3 "" [Connection [1] [2] "c", Connection [2]
        [1] "", Connection [2] [0] ""] [1]
      where
        e = InnerMostState 1 "C" ""
        f = InnerMostState 2 "D" ""
        g = InnerMostState 0 "E" ""
    d = InnerMostState 4 "X" ""

forCheckSameConnection2 :: UMLStateDiagram
forCheckSameConnection2 = StateDiagram [a, b, c, d, l] 1 "" [Connection [1] [2] "a", Connection
           [2, 1, 2] [4] "h", Connection [2, 1, 3] [3] "", Connection [2, 2, 2]
           [3] "", Connection [3] [4] "g",Connection [4] [5] ""] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, i] 1 "" [Connection [1] [2] "b", Connection
            [1] [3] "b"] [1]
          where
            g = InnerMostState 1 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
        f = StateDiagram [j, k] 2 "" [Connection [1] [2] "e"] [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    c = Joint 3
    d = InnerMostState 4 "G" ""
    l = EndState 5

forCheckEmptyTran1 :: UMLStateDiagram
forCheckEmptyTran1 = StateDiagram [a,b,c,d,e,f] 1 "order of management system" [Connection [1] [2] "", Connection [2] [3] "Action",
   Connection [3] [4] "Confirm order(Event)", Connection [4] [5] "",Connection [5] [6] "",Connection [5] [6] "complete"] [1]
    where
      a = InnerMostState 1 "idle" ""
      b = InnerMostState 2 "Send order request" ""
      c = InnerMostState 3 "Select normal or special order" ""
      d = InnerMostState 4 "Order confirmation" ""
      e = InnerMostState 5 "Dispatch order" ""
      f = EndState 6

forCheckEmptyTran2 :: UMLStateDiagram
forCheckEmptyTran2 = StateDiagram [a, b, c] 1 "" [Connection [1] [3] "k", Connection [2]
         [3] "k", Connection [3, 2] [2] "h", Connection [2] [1, 2, 2] "h"] [3]
  where
    a = CombineDiagram [d, e] 1
      where
        d = StateDiagram [h, i] 1 "" [Connection [1] [2] "a", Connection [2]
            [1] "a"] [1]
          where
            h = InnerMostState 1 "A" ""
            i = InnerMostState 2 "B" ""
        e = StateDiagram [j, k,m] 2 "" [Connection [1] [2] "b", Connection [1]
            [3] ""] [1]
          where
            j = InnerMostState 1 "C" ""
            k = InnerMostState 2 "D" ""
            m = InnerMostState 3 "H" ""
    b = InnerMostState 2 "E" ""
    c = StateDiagram [f, g] 3 "" [Connection [1] [2] "a", Connection [2] [1]
        "a"] [1]
      where
        f = InnerMostState 1 "F" ""
        g = InnerMostState 2 "G" ""