module CounterExample where
import Datatype
-- CounterExample for  "Start States checker"
nonExist :: UMLStateDiagram
nonExist = StateDiagram [InnerMostState 1 "" "", InnerMostState 2 "" ""] 1 "" [] [1,2]

outerStateDiag1 :: UMLStateDiagram
outerStateDiag1 = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [1,5,2]
         -- changed this line [1,5,2]
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

outerCombineDiag1:: UMLStateDiagram
outerCombineDiag1 = CombineDiagram [a,b,c] 1
  where
   a = StateDiagram  [d,e] 1 ""  [Connection [1] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [5]
            -- changed this line [5]
     where d = InnerMostState 1 "NumLockOff" ""
           e = InnerMostState 2 "NumLockOn" ""
   b = StateDiagram  [f,g] 2 ""  [Connection [1] [2] "EvCapsLockPressed", Connection [2] [1] "EvCapsLockPressed"] [1]
    where f = InnerMostState 1 "CapsLockOff" ""
          g = InnerMostState 2 "CapsLockOn" ""
   c = StateDiagram  [h,i] 3 ""  [Connection [1] [2] "EvScrollLockPressed", Connection [2] [1] "EvScrollLockPressed"] [1]
    where h = InnerMostState 1 "ScrollLockOff" ""
          i = InnerMostState 2 "ScrollLockOn" ""

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

outerCombineDiagC1 :: UMLStateDiagram
outerCombineDiagC1 = CombineDiagram [a,b,c] 1
  where
   a = StateDiagram  [d,e] 1 ""  [Connection [4] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
    -- Connection [4] [2] "EvNumLockPressed"
     where d = InnerMostState 1 "NumLockOff" ""
           e = InnerMostState 2 "NumLockOn" ""
   b = StateDiagram  [f,g] 2 ""  [Connection [1] [2] "EvCapsLockPressed", Connection [2] [1] "EvCapsLockPressed"] [1]
    where f = InnerMostState 1 "CapsLockOff" ""
          g = InnerMostState 2 "CapsLockOn" ""
   c = StateDiagram  [h,i] 3 ""  [Connection [1] [2] "EvScrollLockPressed", Connection [2] [1] "EvScrollLockPressed"] [1]
    where h = InnerMostState 1 "ScrollLockOff" ""
          i = InnerMostState 2 "ScrollLockOn" ""

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

-- CounterExample for  "Local Uniqueness checker"

outerStateDiagL1 :: UMLStateDiagram
outerStateDiagL1 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [3] "a", Connection
           [1] [2] "f", Connection [2] [4] "b", Connection [3] [4] "c",
           Connection [3] [1] "e", Connection [4] [3] "d", Connection [4] [1]
           "e"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 1 "B" "" -- changed this line InnerMostState 1 "B" ""
    c = InnerMostState 3 "C" ""
    d = InnerMostState 4 "D" ""

outerStateDiagL2 :: UMLStateDiagram
outerStateDiagL2 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2] "a", Connection
               [2, 1, 2] [4] "h", Connection [2, 1, 3] [3] "", Connection [2, 2, 2]
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
        c = Joint 4
         -- changed this line   Joint 4
        d = InnerMostState 4 "G" ""

outerStateDiagL3 :: UMLStateDiagram
outerStateDiagL3 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2] "a", Connection
                       [2, 1, 2] [4] "h", Connection [2, 1, 3] [3] "", Connection [2, 2, 2]
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
insideStateDiagL1 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
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
           d = StateDiagram  [f,g] 2 "state 2" [Connection [1] [2] ""] [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  1 "State 2b" "" -- changed this line
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

insideStateDiagL3::UMLStateDiagram
insideStateDiagL3 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
     where
      a = StateDiagram  [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 1 "state 2" [Connection [1] [2] ""] [1] -- changed this line
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

outerCombineDiagL1 :: UMLStateDiagram
outerCombineDiagL1 = CombineDiagram [a,b,c] 1
  where
   a = StateDiagram  [d,e] 1 ""  [Connection [1] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1]
     where d = InnerMostState 1 "NumLockOff" ""
           e = InnerMostState 2 "NumLockOn" ""
   b = StateDiagram  [f,g] 2 ""  [Connection [1] [2] "EvCapsLockPressed", Connection [2] [1] "EvCapsLockPressed"] [1]
    where f = InnerMostState 1 "CapsLockOff" ""
          g = InnerMostState 2 "CapsLockOn" ""
   c = StateDiagram  [h,i] 2 ""  [Connection [1] [2] "EvScrollLockPressed", Connection [2] [1] "EvScrollLockPressed"] [1] -- changed this line
    where h = InnerMostState 1 "ScrollLockOff" ""
          i = InnerMostState 2 "ScrollLockOn" ""

insideCombineDiagL1 :: UMLStateDiagram
insideCombineDiagL1  = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [1,2,2]
  where
    a = StateDiagram  [d,e] 1 ""  [Connection [4] [2] "EvNumLockPressed", Connection [2] [1] "EvNumLockPressed"] [1] -- changed this line
      where d = InnerMostState 1 "NumLockOff" ""
            e = InnerMostState 2 "NumLockOn" ""
    b = StateDiagram  [f,g] 1 ""  [Connection [1] [2] "EvCapsLockPressed", Connection [2] [1] "EvCapsLockPressed"] [1] -- changed this line
      where f = InnerMostState 1 "CapsLockOff" ""
            g = InnerMostState 2 "CapsLockOn" ""
    c = StateDiagram  [h,i] 1 ""  [Connection [1] [2] "EvScrollLockPressed", Connection [2] [1] "EvScrollLockPressed"] [1] -- changed this line
      where h = InnerMostState 1 "ScrollLockOff" ""
            i = InnerMostState 2 "ScrollLockOn" ""
