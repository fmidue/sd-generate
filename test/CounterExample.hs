module CounterExample where
import Datatype (
  Connection(..),
  HistoryType(..),
  StateDiagram(..),
  UMLStateDiagram,
  )

{-# ANN module "Hlint: ignore Reduce duplication" #-}

bogusExample :: UMLStateDiagram
bogusExample = StateDiagram [CombineDiagram [a,b] 1] 0 "" [Connection [1,2,0] [1,3,0] ""] []
    where
      a = StateDiagram [InnerMostState 0 "" ""] 2 "" [] []
      b = StateDiagram [InnerMostState 0 "" ""] 3 "" [] []

forCheckJoint1 :: UMLStateDiagram
forCheckJoint1 = StateDiagram [a,b,c,d,e,f,g] 1 "" [Connection[5] [1] "a",Connection[1] [2] "a",Connection[2] [3] "",
      Connection[3] [6] "",Connection[5] [4] "",Connection[4] [6] "",Connection[6] [7] ""] [5]
     where a = InnerMostState  1 "Tasse nehmen" ""
           b = InnerMostState  2 "Kaffee trinken" ""
           c = InnerMostState  3 "Tasse absetzen" ""
           d = InnerMostState  4 "Zeitung lesen" ""
           e = Joint 5
           f = Joint 6
           g = EndState 7

forCheckJoint2 :: UMLStateDiagram
forCheckJoint2 = StateDiagram [a,b,c,d,e,f,g] 1 "" [Connection[5] [1] "a",Connection[1] [2] "a",Connection[2] [3] "",
      Connection[3] [6] "a",Connection[5] [4] "a",Connection[4] [6] "",Connection[6] [7] ""] [5]
     where a = InnerMostState  1 "Tasse nehmen" ""
           b = InnerMostState  2 "Kaffee trinken" ""
           c = InnerMostState  3 "Tasse absetzen" ""
           d = InnerMostState  4 "Zeitung lesen" ""
           e = Joint 5
           f = Joint 6
           g = EndState 7

forCheckEndState1 :: UMLStateDiagram
forCheckEndState1 = StateDiagram [a,b,c,d,e,f,g] 1 "" [Connection[5] [1] "",Connection[1] [2] "",Connection[2] [3] "",
      Connection[3] [6] "",Connection[5] [4] "",Connection[4] [6] "",Connection[7] [6] ""] [5]
     where a = InnerMostState  1 "Tasse nehmen" ""
           b = InnerMostState  2 "Kaffee trinken" ""
           c = InnerMostState  3 "Tasse absetzen" ""
           d = InnerMostState  4 "Zeitung lesen" ""
           e = Joint 5
           f = Joint 6
           g = EndState 7

forCheckEndState2 :: UMLStateDiagram
forCheckEndState2 = StateDiagram [a, b] 1 "" [Connection [1,3] [2] "a"] []
  where
    a = StateDiagram [c, d, e] 1 "A" [Connection [1] [2] "", Connection [2]
        [3] "", Connection [3] [2] "", Connection [2] [1] "a"] [1]
      where
        c = InnerMostState 1 "B" ""
        d = InnerMostState 2 "C" ""
        e = EndState 3
    b = InnerMostState 2 "E" ""


nonExist :: UMLStateDiagram
nonExist = StateDiagram [InnerMostState 1 "" "", InnerMostState 2 "" ""] 1 "" [] [1,2]

outerStateDiag1 :: UMLStateDiagram
outerStateDiag1 = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [1,5,2]  -- changed this line [1,5,2]
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

outerStateDiag2 :: UMLStateDiagram
outerStateDiag2 = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [5]
        -- changed this line [5]
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

innerStateDiag1 :: UMLStateDiagram
innerStateDiag1 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [5]
      -- changed this line [5]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [2] ""] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

innerStateDiag2 :: UMLStateDiagram
innerStateDiag2 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [2,1,3]
      -- changed this line [2,1,3]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [2] ""] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

innerCombineDiag1:: UMLStateDiagram
innerCombineDiag1= StateDiagram [a, b, c, d] 1 "" [Connection [1] [2] "a", Connection
           [2, 1, 2] [4] "h", Connection [2, 1, 3] [3] "", Connection [2, 2, 2]
           [3] "", Connection [3] [4] "g"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, i] 1 "" [Connection [1] [2] "b", Connection
            [2] [3] "c"] [5]
            -- changed this line [5]
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

-- CounterExample for  "Connection Points checker"

outerStateDiagC1 :: UMLStateDiagram
outerStateDiagC1 = StateDiagram [a, b, c, d] 1 "" [Connection [5] [3] "a", Connection
-- changed this line Connection [5] [3] "a"
           [1] [2] "f", Connection [2] [4] "b", Connection [3] [4] "c",
           Connection [3] [1] "e", Connection [4] [3] "d", Connection [4] [1]
           "e"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""
    c = InnerMostState 3 "C" ""
    d = InnerMostState 4 "D" ""

outerStateDiagC2 :: UMLStateDiagram
outerStateDiagC2 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2] "a", Connection
               [2, 1, 2] [4] "h", Connection [2, 1, 4] [3] "", Connection [2, 2, 2]
               -- changed this line Connection [2, 1, 4] [3] ""
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

outerStateDiagC3 :: UMLStateDiagram
outerStateDiagC3 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2] "a", Connection
                       [2, 1, 2] [4] "h", Connection [2, 4, 3] [3] "", Connection [2, 2, 2]
                       -- changed this line Connection [2, 4, 3] [3] ""
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

smallTestC :: UMLStateDiagram
smallTestC = StateDiagram [a,b] 1 "" [Connection [1, 4, 3] [2] ""] [1]
        where
          a = CombineDiagram [e, f] 1
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
          b = InnerMostState 2 "" ""

smallTestC1 :: UMLStateDiagram
smallTestC1 = StateDiagram [a,b] 1 "" [Connection [1,1,4] [2] ""] [1]
        where
          a = CombineDiagram [e, f] 1
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
          b = InnerMostState 2 "" ""

insideStateDiagC1 :: UMLStateDiagram
insideStateDiagC1 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
      where
       a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [5] [2] ""] [1]
       -- changed this line Connection [5] [2] ""
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [2] ""] [1]
            where
             f = InnerMostState  1 "State 2a" ""
             g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
       b = InnerMostState  2 "State 3" ""

insideStateDiagC2::UMLStateDiagram
insideStateDiagC2 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [99] ""] [1]
           -- changed this line Connection [1] [99] ""
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

insideStateDiagC3::UMLStateDiagram
insideStateDiagC3 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [0] ""] [1]
           -- changed this line Connection [1] [0] ""
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""


insideCombineDiagC1 :: UMLStateDiagram
insideCombineDiagC1 = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [1,2,2]
  where
   a = StateDiagram  [d,e] 1 ""  [Connection [4] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
   -- changed this line Connection [4] [2] "EvNumLockPressed"
     where d = InnerMostState 1 "NumLockOff" ""
           e = InnerMostState 2 "NumLockOn" ""
   b = StateDiagram  [f,g] 2 ""  [Connection [1] [2] "EvCapsLockPressed", Connection [2] [1] "EvCapsLockPressed"] [1]
    where f = InnerMostState 1 "CapsLockOff" ""
          g = InnerMostState 2 "CapsLockOn" ""
   c = StateDiagram  [h,i] 3 ""  [Connection [1] [2] "EvScrollLockPressed", Connection [2] [1] "EvScrollLockPressed"] [1]
    where h = InnerMostState 1 "ScrollLockOff" ""
          i = InnerMostState 2 "ScrollLockOn" ""

tooDeep :: UMLStateDiagram
tooDeep = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [1,2,2]
  where
   a = StateDiagram  [d,e] 1 ""  [Connection [2,3] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
   -- changed this line Connection [2,3] [2] "EvNumLockPressed"
     where d = InnerMostState 1 "NumLockOff" ""
           e = InnerMostState 2 "NumLockOn" ""
   b = StateDiagram  [f,g] 2 ""  [Connection [1] [2] "EvCapsLockPressed", Connection [2] [1] "EvCapsLockPressed"] [1]
     where f = InnerMostState 1 "CapsLockOff" ""
           g = InnerMostState 2 "CapsLockOn" ""
   c = StateDiagram  [h,i] 3 ""  [Connection [1] [2] "EvScrollLockPressed", Connection [2] [1] "EvScrollLockPressed"] [1]
     where h = InnerMostState 1 "ScrollLockOff" ""
           i = InnerMostState 2 "ScrollLockOn" ""

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

-- CounterExample for  "Local Uniqueness checker"
outerStateDiagL1 :: UMLStateDiagram
outerStateDiagL1 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [3] "a", Connection
           [1] [1] "f", Connection [1] [4] "b", Connection [3] [4] "c",
           Connection [3] [1] "e", Connection [4] [3] "d", Connection [4] [1]
           "e"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 1 "B" "" -- changed this line InnerMostState 1 "B" ""
    c = InnerMostState 3 "C" ""
    d = InnerMostState 4 "D" ""

outerStateDiagL2 :: UMLStateDiagram
outerStateDiagL2 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2] "a", Connection
               [2, 1, 2] [4] "", Connection [2, 1, 3] [4] "", Connection [2, 2, 2]
               [4] "", Connection [4] [4] ""] [1]
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
        c = Joint 4
         -- changed this line   Joint 4
        d = InnerMostState 4 "G" ""

outerStateDiagL3 :: UMLStateDiagram
outerStateDiagL3 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [4] "a", Connection [4]
                       [3] "", Connection [3] [4] "g"] [1]
        where
          a = InnerMostState 1 "A" ""
          b = CombineDiagram [e, f] 1  -- changed this line
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

insideStateDiagL1 :: UMLStateDiagram
insideStateDiagL1 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1] ""] []
      where
       a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [2] ""] [1]
            where
             f = InnerMostState  1 "State 2a" ""
             g = InnerMostState  2 "State 2b" ""
           e = History 2 Deep                     -- changed this line
       b = InnerMostState  2 "State 3" ""

insideStateDiagL2::UMLStateDiagram
insideStateDiagL2 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
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

insideStateDiagL3::UMLStateDiagram
insideStateDiagL3 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 1 "state 2" [Connection [1] [2] ""] [1] -- changed this line
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

insideCombineDiagL1 :: UMLStateDiagram
insideCombineDiagL1  = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [1]
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

-- CounterExample for checkStructure/checkOuterMostLayer
outerMostCombineDiag :: UMLStateDiagram
outerMostCombineDiag = CombineDiagram [a,b,c] 1
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

-- CounterExample for checkStructure/checkSubstateSD
substateOnlyJH1::UMLStateDiagram
substateOnlyJH1 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1] ""] []
     where
      a = Joint 1
      b = Joint 2

substateOnlyJH2::UMLStateDiagram
substateOnlyJH2 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1] ""] []
    where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
          where
           c = History 1 Deep
           d = Joint 2
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

-- CounterExample for checkStructure/checkSubstateCD
oneSD1 :: UMLStateDiagram
oneSD1 = StateDiagram [CombineDiagram [a] 1] 1 "active" [] [1]
  where
    a = StateDiagram  [d,e] 1 ""  [Connection [1] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
      where d = InnerMostState 1 "NumLockOff" ""
            e = InnerMostState 2 "NumLockOn" ""

oneSD2 :: UMLStateDiagram
oneSD2 = StateDiagram [CombineDiagram [a,b] 1] 1 "active" [] [1]
  where
    a = StateDiagram  [d,e] 1 ""  [Connection [1] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
      where d = InnerMostState 1 "NumLockOff" ""
            e = InnerMostState 2 "NumLockOn" ""
    b = InnerMostState 2 "" ""

forCheckHistOutTransition1 :: UMLStateDiagram
forCheckHistOutTransition1 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[1,3] [2] "error"] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [2] ""] [1]
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
        Connection [1, 3] [2] "c", Connection [1, 2, 2] [2, 2] "h", Connection
        [2, 2] [1, 1, 2, 3] "e", Connection [1,0] [2, 1] "f"] []
      where
        b = StateDiagram [g, h, i, j] 1 "B" [Connection [1, 1, 2] [3] "",
            Connection [1, 2, 2] [3] "", Connection [0] [2] ""] []
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
        e = StateDiagram [j, k] 2 "" [Connection [] [2] "b", Connection [2]
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
        g = History 0 Shallow
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
