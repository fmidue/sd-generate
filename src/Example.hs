module Example where
import Datatype (
  Connection(..),
  HistoryType(..),
  StateDiagram(..),
  UMLStateDiagram,
  )

{-# ANN module "Hlint: ignore Reduce duplication" #-}
  
verySmall :: UMLStateDiagram
verySmall = StateDiagram [EndState 1] 0 "" [] [1]

picture1 :: UMLStateDiagram
picture1 = StateDiagram [CombineDiagram [a,b,c] 1] 1 "active" [] [1,2,2]
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

picture2 :: UMLStateDiagram
picture2 = StateDiagram [a,b,c,d,e,f] 1 "order of management system" [Connection [1] [2] "", Connection [2] [3] "Action",
   Connection [3] [4] "Confirm order(Event)", Connection [4] [5] "",Connection [2] [6] "exit",Connection [5] [6] "complete"] [1]
    where
      a = InnerMostState 1 "idle" ""
      b = InnerMostState 2 "Send order request" ""
      c = InnerMostState 3 "Select normal or special order" ""
      d = InnerMostState 4 "Order confirmation" ""
      e = InnerMostState 5 "Dispatch order" ""
      f = EndState 6

picture3 :: UMLStateDiagram
picture3 = StateDiagram [a,b,c,d] 1 "" [Connection[3] [1,1,1] "",Connection[1,1,1] [1,1,2] "",
       Connection[1,1,2] [1,1,3] "",Connection[1,1,3] [4] "",Connection[3] [1,2,1] "",
       Connection[1,2,1] [4] "",Connection[4] [5] "b"] [3]
       where 
        a = CombineDiagram [e,f] 1
         where
            e = StateDiagram [g,h,i] 1 "" [] []
              where 
                g = InnerMostState 1 "Tasse nehmen" ""
                h = InnerMostState 2 "Kaffee trinken" ""
                i = InnerMostState 3 "Tasse absetzen" ""
            f = StateDiagram [InnerMostState 1 "Zeitung lesen" ""] 2 "" [] []
        b = Joint 3
        c = Joint 4
        d = EndState 5

picture4 :: UMLStateDiagram
picture4 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t",Connection[2] [1,3] ""] []
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

slide246 :: UMLStateDiagram
slide246 = StateDiagram a 1 "" (map (\x -> Connection [x] [x+1]
           "after(1min)") [1..59] ++ [Connection [60] [1] "after(1min)"]) [1]
  where
    a = map (\x -> InnerMostState x (show(x-1)) "") [1..60]

slide253 :: UMLStateDiagram
slide253 = StateDiagram [CombineDiagram [a, b] 1] 1 "" [] []
  where
    a = StateDiagram c 1 "Stunden" (map (\x -> Connection [x] [x+1]
           "h") [1..23] ++ [Connection [24] [1] "h"]) [1]
      where
        c = map (\x -> InnerMostState x (show(x-1)) "") [1..24]
    b = StateDiagram d 2 "Minuten" (map (\x -> Connection [x] [x+1]
           "after(1min)") [1..59] ++ [Connection [60] [1] "after(1min)"]) [1]
      where
        d = InnerMostState 1 "0" "entry/beep" : map (\x -> InnerMostState x (show(x-1)) "")
          [2..60]

slide257 :: UMLStateDiagram
slide257 = StateDiagram [a, b] 1 "" [Connection [1] [2] "a", Connection [2]
           [1] "a"] [2]
  where
    a = StateDiagram [c, d] 1 "Alarm" [Connection [1] [2] "b", Connection [2]
        [1] "b"] [1]
      where
        c = InnerMostState 1 "on" ""
        d = InnerMostState 2 "off" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram c 1 "Stunden" (map (\x -> Connection [x] [x+1]
              "h") [1..23] ++ [Connection [24] [1] "h"]) [1]
          where
            c = map (\x -> InnerMostState x (show(x-1)) "") [1..24]

        f = StateDiagram d 2 "Minuten" (map (\x -> Connection [x] [x+1]
            "after(1min)") [1..59] ++ [Connection [60] [1] "after(1min)"]) [1]
          where
            d = InnerMostState 1 "0" "entry/beep" :
              map (\x -> InnerMostState x (show(x-1)) "") [2..60]

slide267a :: UMLStateDiagram
slide267a = StateDiagram [a, b] 1 "" [Connection [1] [2] "a"] []
  where
    a = StateDiagram [c, d, e] 1 "A" [Connection [1] [2] "", Connection [2]
        [3] "b", Connection [3] [2] "", Connection [2] [1] "a"] [1]
      where
        c = InnerMostState 1 "B" ""
        d = InnerMostState 2 "C" ""
        e = InnerMostState 3 "D" ""
    b = InnerMostState 2 "E" ""

slide267b :: UMLStateDiagram
slide267b = StateDiagram [a, b] 1 "" [Connection [1,1] [2] "a", Connection
            [1, 2] [2] "a", Connection [1, 3] [2] "a"] []
  where
    a = StateDiagram [c, d, e] 1 "A" [Connection [1] [2] "b", Connection [2]
        [3] "c", Connection [3] [2] "b", Connection [2] [1] "b"] [1]
      where
        c = InnerMostState 1 "B" ""
        d = InnerMostState 2 "C" ""
        e = InnerMostState 3 "D" ""
    b = InnerMostState 2 "E" ""

slide271 :: UMLStateDiagram
slide271 = StateDiagram [a, b, c] 1 "" [Connection [3] [1, 0] "a",
           Connection [1] [2] "a", Connection [2] [3, 1, 0] "", Connection [2]
           [3, 2, 0] ""] [3]
  where
    a = StateDiagram [d, e, f] 1 "Alarm" [Connection [1] [2] "b", Connection
        [2] [1] "b", Connection [0] [1] ""] [1]
      where
        d = InnerMostState 1 "on" ""
        e = InnerMostState 2 "off" ""
        f = History 0 Shallow
    b = Joint 2
    c = CombineDiagram [g, h] 3
      where
        g = StateDiagram (i ++ [k]) 1 "Stunden" (map (\x -> Connection [x]
            [x+1] "h") [1..23] ++ [Connection [24] [1] "h", Connection [0] [1]
            ""]) [1]
          where
            i = map (\x -> InnerMostState x (show(x-1)) "") [1..24]
            k = History 0 Shallow

        h = StateDiagram (j ++ [l]) 2 "Minuten" (map (\x -> Connection [x]
            [x+1] "after(1min)") [1..59] ++ [Connection [60] [1] "after(1min)",
            Connection [0] [1] ""]) [1]
          where
            j = InnerMostState 1 "0" "entry/beep" :
              map (\x -> InnerMostState x (show(x-1)) "") [2..60]
            l = History 0 Shallow

slide273 :: UMLStateDiagram
slide273 = StateDiagram [a, b, c] 1 "" [Connection [3] [1, 1] "a [al==1]",
           Connection [3] [1, 2] "a [al==0]", Connection [1] [2] "a",
           Connection [2] [3, 1, 0] "", Connection [2] [3, 2, 0] ""] [3]
  where
    a = StateDiagram [d, e] 1 "Alarm" [Connection [1] [2] "b", Connection
        [2] [1] "b"] [1]
      where
        d = InnerMostState 1 "on" ""
        e = InnerMostState 2 "off" ""
    b = Joint 2
    c = CombineDiagram [g, h] 3
      where
        g = StateDiagram (i ++ [k]) 1 "Stunden" (map (\x -> Connection [x]
            [x+1] "h") [1..23] ++ [Connection [24] [1] "h", Connection [0] [1]
            ""]) [1]
          where
            i = map (\x -> InnerMostState x (show(x-1)) "") [1..24]
            k = History 0 Shallow

        h = StateDiagram (j ++ [l]) 2 "Minuten" (map (\x -> Connection [x]
            [x+1] "after(1min)") [1..59] ++ [Connection [60] [1] "after(1min)",
            Connection [0] [1] ""]) [1]
          where
            j = InnerMostState 1 "0" "entry [al==1] / beep" :
              map (\x -> InnerMostState x (show(x-1)) "") [2..60]
            l = History 0 Shallow

slide275 :: UMLStateDiagram
slide275 = StateDiagram [a, b] 1 "" [Connection [1] [2] "Batterie wird leer",
           Connection [2] [1] "Neue Batterie wird eingesetzt / al=1"] [2]
  where
    a = slide273
    b = InnerMostState 2 "Batterie(fach) leer" ""

slide277 :: UMLStateDiagram
slide277 = StateDiagram [a, b, c] 1 "" [Connection [1] [2] "f", Connection
           [1] [3] "a", Connection [2] [3, 2] "b", Connection [3] [1] "e"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""
    c = StateDiagram [d, e] 3 "" [Connection [1] [2] "c", Connection [2] [1]
        "d"] [1]
      where
        d = InnerMostState 1 "C" ""
        e = InnerMostState 2 "D" ""

slide278 :: UMLStateDiagram
slide278 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [3] "a", Connection
           [1] [2] "f", Connection [2] [4] "b", Connection [3] [4] "c",
           Connection [3] [1] "e", Connection [4] [3] "d", Connection [4] [1]
           "e"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""
    c = InnerMostState 3 "C" ""
    d = InnerMostState 4 "D" ""

slide279 :: UMLStateDiagram
slide279 = StateDiagram [a, b, c, d, l] 1 "" [Connection [1] [2] "a", Connection
           [2, 1, 2] [4] "h", Connection [2, 1, 3] [3] "", Connection [2, 2, 2]
           [3] "", Connection [3] [4] "g",Connection [4] [5] ""] [1]
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
    l = EndState 5

slide280 :: UMLStateDiagram
slide280 = StateDiagram [a, b, c, d, e, f, g, h] 1 "" [Connection [1] [2]
           "a", Connection [2] [3] "b", Connection [2] [5] "e", Connection [3]
           [4] "c", Connection [3] [6] "e", Connection [4] [7] "e", Connection
           [5] [6] "b", Connection [6] [7] "c", Connection [6] [8] "h",
           Connection [7] [8] "g", Connection [3] [8] "h"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "(B, E)" ""
    c = InnerMostState 3 "(C, E)" ""
    d = InnerMostState 4 "(D, E)" ""
    e = InnerMostState 5 "(B, F)" ""
    f = InnerMostState 6 "(C, F)" ""
    g = InnerMostState 7 "(D, F)" ""
    h = InnerMostState 8 "G" ""

slide281 :: UMLStateDiagram
slide281 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2] "a", Connection
           [2] [1] "f", Connection [2] [3, 0] "b", Connection [3] [1] "e",
           Connection [4] [3] "x"] [4]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""
    c = StateDiagram [e, f, g] 3 "" [Connection [1] [2] "c", Connection [2]
        [1] "d", Connection [0] [1] ""] [1]
      where
        e = InnerMostState 1 "C" ""
        f = InnerMostState 2 "D" ""
        g = History 0 Shallow
    d = InnerMostState 4 "X" ""

slide283 :: UMLStateDiagram
slide283 = StateDiagram [a, b, c, d, e, f, g] 1 "" [Connection [1] [2] "a",
           Connection [2] [1] "f", Connection [3] [4] "a", Connection [4] [3]
           "f", Connection [2] [5] "b", Connection [4] [6] "b", Connection [5]
           [6] "c", Connection [6] [5] "d", Connection [6] [3] "e", Connection
           [5] [1] "e", Connection [7] [5] "x"] [7]
  where
    a = InnerMostState 1 "A(C)" ""
    b = InnerMostState 2 "B(C)" ""
    c = InnerMostState 3 "A(D)" ""
    d = InnerMostState 4 "B(D)" ""
    e = InnerMostState 5 "C" ""
    f = InnerMostState 6 "D" ""
    g = InnerMostState 7 "X()" ""

task26a :: UMLStateDiagram
task26a = StateDiagram [a, b, c, d] 1 "" [Connection [1, 1, 3] [2] "e",
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
    d = StateDiagram [g, h] 4 "" [Connection [2] [1] "d"] [2]
      where
        g = InnerMostState 1 "G" ""
        h = InnerMostState 2 "H" ""

task26b :: UMLStateDiagram
task26b = StateDiagram [a, b, c, d, e, f, g, h, i] 1 "" [Connection [1] [2]
          "a", Connection [1] [4] "b", Connection [2] [6] "b", Connection [3]
          [6] "b", Connection [3] [1] "a", Connection [4] [1] "b", Connection
          [4] [5] "a", Connection [5] [3] "b", Connection [6] [3] "b",
          Connection [6] [4] "a", Connection [6] [7] "c", Connection [7] [9]
          "c", Connection [7] [9] "d", Connection [9] [8] "d", Connection [8]
          [1] "a", Connection [9] [4] "a"] [2]
  where
    a = InnerMostState 1 "(A, D)" ""
    b = InnerMostState 2 "(B, D)" ""
    c = InnerMostState 3 "(C, D)" ""
    d = InnerMostState 4 "(A, E)" ""
    e = InnerMostState 5 "(B, E)" ""
    f = InnerMostState 6 "(C, E)" ""
    g = InnerMostState 7 "F" ""
    h = InnerMostState 8 "G" ""
    i = InnerMostState 9 "H" ""

task27 :: UMLStateDiagram
task27 = StateDiagram [a, b] 1 "" [Connection [1] [2, 0] "F", Connection [2]
         [1] "F"] [1]
  where
    a = InnerMostState 1 "CD" ""
    b = StateDiagram [c, d, e, f] 2 "Radio" [Connection [0] [1] "",
        Connection [1] [2] "+", Connection [2] [3] "+", Connection [3] [2] "-",
        Connection [2] [1] "-"] []
      where
        c = InnerMostState 1 "1" ""
        d = InnerMostState 2 "2" ""
        e = InnerMostState 3 "3" ""
        f = History 0 Shallow

-- task28 is the same as task26a

task85 :: UMLStateDiagram
task85 = StateDiagram [Joint 1, a] 1 "" [Connection [1] [2, 1, 1, 1, 1] "",
         Connection [1] [2, 1, 1, 2, 1] ""] [1]
  where
    a = StateDiagram [b, c] 2 "A" [Connection [1, 1, 1, 1] [2] "i",
        Connection [1, 3] [2] "", Connection [1, 2, 2] [2, 2] "h", Connection
        [2, 2] [1, 1, 2, 3] "e", Connection [2, 1] [1, 0] "f"] []
      where
        b = StateDiagram [g, h, i, j] 1 "B" [Connection [1, 1, 2] [3] "c",
            Connection [1, 2, 2] [3] "c", Connection [0] [2] ""] []
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

task88 :: UMLStateDiagram
task88 = StateDiagram [a, b, c] 1 "" [Connection [1] [3] "k", Connection [2]
         [3] "k", Connection [3, 2] [2] "h", Connection [2] [1, 2, 2] "h"] [3]
  where
    a = CombineDiagram [d, e] 1
      where
        d = StateDiagram [h, i] 1 "" [Connection [1] [2] "a", Connection [2]
            [1] "a"] [1]
          where
            h = InnerMostState 1 "A" ""
            i = InnerMostState 2 "B" ""
        e = StateDiagram [j, k] 2 "" [Connection [1] [2] "b", Connection [2]
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


test2 :: UMLStateDiagram
test2 = StateDiagram [a, b] 1 "" [Connection [1] [2, 2, 2] "a"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = StateDiagram [c, d] 2 "B" [Connection [1] [2] "b"] [1]
    c = InnerMostState 1 "C" ""
    d = StateDiagram [e, f] 2 "D" [Connection [1] [2] "c"] [1]
    e = InnerMostState 1 "E" ""
    f = InnerMostState 2 "F" ""

test4 :: UMLStateDiagram
test4 = StateDiagram [a] 1 "" [] [1, 1, 1]
  where
    a = StateDiagram [b, c] 1 "A" [Connection [1, 2, 1, 1] [2] "i", Connection [1, 3] [2] "",
      Connection [2, 2] [1, 2, 2, 3] "e", Connection [1, 5, 2] [2, 2] "h",
      Connection [2, 1] [1, 4] "f"] []
    b = StateDiagram [b1, b2, b3, b4, b5] 1 "B" [Connection [4] [5] "", Connection [1] [2, 1, 1] "",
      Connection [1] [2, 2, 3] "", Connection [2, 1, 2] [3] "c", Connection [2, 2, 2] [3] "c"] []
    c = StateDiagram [c1, c2, c3] 2 "E" [Connection [1] [3] "d", Connection [3] [2] "d",
      Connection [2] [1] "d"] [3]
    c1 = InnerMostState 1 "6" ""
    c2 = InnerMostState 2 "7" ""
    c3 = InnerMostState 3 "8" ""
    b1 = Joint 1
    b2 = CombineDiagram [d1, d2] 2
    b3 = Joint 3
    b4 = History 4 Shallow
    b5 = StateDiagram [e1, e2] 5 "D" [Connection [1] [2] "g", Connection [2] [1] "g"] [1]
    e1 = InnerMostState 1 "9" ""
    e2 = InnerMostState 2 "10" ""
    d1 = StateDiagram [f1, f2] 1 "C" [Connection [1] [2] "a", Connection [2] [1] "a"] [1]
    d2 = StateDiagram [g1, g2, g3] 2 "" [Connection [1] [2] "b", Connection [2] [3] "b",
      Connection [3] [1] "b"] [1]
    f1 = InnerMostState 1 "1" ""
    f2 = InnerMostState 2 "2" ""
    g1 = InnerMostState 1 "3" ""
    g2 = InnerMostState 2 "4" ""
    g3 = InnerMostState 3 "5" ""
