module Example where
import Datatype (
  Connection(..),
  HistoryType(..),
  StateDiagram(..),
  UMLStateDiagram,
  umlStateDiagram,
  )

{-# ANN module "Hlint: ignore Reduce duplication" #-}

verySmall :: UMLStateDiagram String Int
verySmall = umlStateDiagram $
            StateDiagram [EndState 1] 1 "" [] [1]

{- Accepted by Alloy directly -}
picture1 :: UMLStateDiagram String Int
picture1 = umlStateDiagram $
           StateDiagram [ CombineDiagram [a,b,c] 1] 12 "active" [] [1,2,2]
  where
   a = StateDiagram  [d,e] 1 ""
       [ Connection [1] [2] "EvNumLockPressed"
       , Connection [2] [1] "EvNumLockPressed"]
       [1]
     where d = InnerMostState 1 "NumLockOff" ""
           e = InnerMostState 2 "NumLockOn" ""
   b = StateDiagram  [f,g] 2 ""
       [ Connection [1] [2] "EvCapsLockPressed"
       , Connection [2] [1] "EvCapsLockPressed"]
       [1]
    where f = InnerMostState 1 "CapsLockOff" ""
          g = InnerMostState 2 "CapsLockOn" ""
   c = StateDiagram  [h,i] 3 ""
       [ Connection [1] [2] "EvScrollLockPressed"
       , Connection [2] [1] "EvScrollLockPressed"]
       [1]
    where h = InnerMostState 1 "ScrollLockOff" ""
          i = InnerMostState 2 "ScrollLockOn" ""

{- Accepted by Alloy directly -}
picture2 :: UMLStateDiagram String Int
picture2 = umlStateDiagram $
           StateDiagram [a,b,c,d,e,f] 7 "order of management system"
           [ Connection [1] [2] ""
           , Connection [2] [3] "Action"
           , Connection [3] [4] "Confirm order(Event)"
           , Connection [4] [5] ""
           , Connection [2] [6] "exit"
           , Connection [5] [6] "complete"]
           [1]
    where
      a = InnerMostState 1 "idle" ""
      b = InnerMostState 2 "Send order request" ""
      c = InnerMostState 3 "Select normal or special order" ""
      d = InnerMostState 4 "Order confirmation" ""
      e = InnerMostState 5 "Dispatch order" ""
      f = EndState 6

{- Accepted by Alloy directly -}
picture3 :: UMLStateDiagram String Int
picture3 = umlStateDiagram $
           StateDiagram [a,b,c,d] 12 ""
           [ Connection[3] [1,1,1] ""
           , Connection[1,1,3] [4] ""
           , Connection[3] [1,2,1] ""
           , Connection[1,2,1] [4] ""
           , Connection[4] [5] "b"] [3]
       where
        a = CombineDiagram [e,f] 1
         where
            e = StateDiagram [g,h,i] 1 ""
               [ Connection[1] [2] ""
               , Connection[2] [3] ""]
               []
              where
                g = InnerMostState 1 "Tasse nehmen" ""
                h = InnerMostState 2 "Kaffee trinken" ""
                i = InnerMostState 3 "Tasse absetzen" ""
            f = StateDiagram [InnerMostState 1 "Zeitung lesen" ""] 2 "" [] []
        b = ForkOrJoin 3
        c = ForkOrJoin 4
        d = EndState 5

{- After some changes, accepted by Alloy -}
picture4 :: UMLStateDiagram String Int
picture4 = umlStateDiagram $
           StateDiagram [a,b] 14 ""
           [ Connection[1] [2] "t"
           , Connection[2] [1,3] "" ]
           [1]
     where
      a = StateDiagram  [c,d,e] 1 "Composite State"
          [ Connection [1] [2] "a" ]
          [1]
          where
           c = InnerMostState  1 "State 1" ""
           d = StateDiagram  [f,g] 2 "state 2"
               [ Connection [1] [2] "b" ]
               [1]
            where
              f = InnerMostState  1 "State 2a" ""
              g = InnerMostState  2 "State 2b" ""
           e = History 3 Deep
      b = InnerMostState  2 "State 3" ""

{- Accepted by Alloy directly -}
slide246 :: UMLStateDiagram String Int
slide246 = umlStateDiagram $ StateDiagram a 86 "" (map (\x -> Connection [x] [x+1]
           "after(1min)") [1..59] ++ [Connection [60] [1] "after(1min)"]) [1]
  where
    a = map (\x -> InnerMostState x (show(x-1)) "") [1..60]

{- Accepted by Alloy directly -}
slide253 :: UMLStateDiagram String Int
slide253 = umlStateDiagram $ StateDiagram [CombineDiagram [a, b] 1] 86 "" [] []
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

{- Alloy analyzer cannot get results for this diagram in short time -}
slide257 :: UMLStateDiagram String Int
slide257 = umlStateDiagram $
           StateDiagram [a, b] 200 ""
           [ Connection [1] [2] "a"
           , Connection [2] [1] "a"]
           [2]
  where
    a = StateDiagram [c, d] 1 "Alarm"
        [ Connection [1] [2] "b"
        , Connection [2] [1] "b"] [1]
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

{- After some changes, accepted by Alloy -}
slide267a :: UMLStateDiagram String Int
slide267a = umlStateDiagram $
            StateDiagram [a, b] 11 ""
            [ Connection [1] [2] "a" ]
            [1]
  where
    a = StateDiagram [c, d, e] 1 "A"
        [ Connection [1] [2] "d"
        , Connection [2] [3] "b"
        , Connection [3] [2] "e"
        , Connection [2] [1] "c"]
        [1]
      where
        c = InnerMostState 1 "B" ""
        d = InnerMostState 2 "C" ""
        e = InnerMostState 3 "D" ""
    b = InnerMostState 2 "E" ""

{- After some changes, accepted by Alloy -}
slide267b :: UMLStateDiagram String Int
slide267b = umlStateDiagram $
            StateDiagram [a, b] 10 ""
            [ Connection [1,1] [2] "a"
            , Connection [1, 2] [2] "a"
            , Connection [1, 3] [2] "a"]
            [1]
  where
    a = StateDiagram [c, d, e] 1 "A"
        [ Connection [1] [2] "b"
        , Connection [2] [3] "c"
        , Connection [3] [2] "b"
        , Connection [2] [1] "b"]
        [1]
      where
        c = InnerMostState 1 "B" ""
        d = InnerMostState 2 "C" ""
        e = InnerMostState 3 "D" ""
    b = InnerMostState 2 "E" ""

{- Alloy analyzer cannot get results for this diagram in short time -}
slide271 :: UMLStateDiagram String Int
slide271 = umlStateDiagram $
           StateDiagram [a, b, c] 200 ""
           [ Connection [3] [1, 0] "a"
           , Connection [1] [2] "a"
           , Connection [2] [3, 1, 0] ""
           , Connection [2] [3, 2, 0] ""]
           [3]
  where
    a = StateDiagram [d, e, f] 1 "Alarm"
        [ Connection [1] [2] "b"
        , Connection [2] [1] "b"
        , Connection [0] [1] ""]
        [1]
      where
        d = InnerMostState 1 "on" ""
        e = InnerMostState 2 "off" ""
        f = History 0 Shallow
    b = ForkOrJoin 2
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

{- Alloy analyzer cannot get results for this diagram in short time -}
slide273 :: UMLStateDiagram String Int
slide273 = umlStateDiagram $
           StateDiagram [a, b, c] 200 ""
           [ Connection [3] [1, 1] "a [al==1]"
           , Connection [3] [1, 2] "a [al==0]"
           , Connection [1] [2] "a"
           , Connection [2] [3, 1, 0] ""
           , Connection [2] [3, 2, 0] ""] [3]
  where
    a = StateDiagram [d, e] 1 "Alarm"
        [ Connection [1] [2] "b"
        , Connection [2] [1] "b"]
        [1]
      where
        d = InnerMostState 1 "on" ""
        e = InnerMostState 2 "off" ""
    b = ForkOrJoin 2
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

{- Alloy analyzer cannot get results for this diagram in short time -}
slide275 :: UMLStateDiagram String Int
slide275 = umlStateDiagram $
           StateDiagram [a, b] 200 ""
           [ Connection [1] [2] "Batterie wird leer"
           , Connection [2] [1] "Neue Batterie wird eingesetzt / al=1"]
           [2]
  where
    a = StateDiagram [c, d, e] 1 ""
        [ Connection [3] [1, 1] "a [al==1]"
        , Connection [3] [1, 2] "a [al==0]"
        , Connection [1] [2] "a"
        , Connection [2] [3, 1, 0] ""
        , Connection [2] [3, 2, 0] ""]
        [3]
      where
        c = StateDiagram [f, g] 1 "Alarm"
            [ Connection [1] [2] "b"
            , Connection [2] [1] "b"]
            [1]
          where
            f = InnerMostState 1 "on" ""
            g = InnerMostState 2 "off" ""
        d = ForkOrJoin 2
        e = CombineDiagram [h, i] 3
          where
            h = StateDiagram (j ++ [k]) 1 "Stunden" (map (\x -> Connection [x]
                [x+1] "h") [1..23] ++ [Connection [24] [1] "h", Connection [0] [1]
                ""]) [1]
              where
                j = map (\x -> InnerMostState x (show(x-1)) "") [1..24]
                k = History 0 Shallow

            i = StateDiagram (l ++ [m]) 2 "Minuten" (map (\x -> Connection [x]
                [x+1] "after(1min)") [1..59] ++ [Connection [60] [1] "after(1min)",
                Connection [0] [1] ""]) [1]
              where
                l = InnerMostState 1 "0" "entry [al==1] / beep" :
                  map (\x -> InnerMostState x (show(x-1)) "") [2..60]
                m = History 0 Shallow
    b = InnerMostState 2 "Batterie(fach) leer" ""

{- Accepted by Alloy directly -}
slide277 :: UMLStateDiagram String Int
slide277 = umlStateDiagram $
           StateDiagram [a, b, c] 11 ""
           [ Connection [1] [2] "f"
           , Connection [1] [3] "a"
           , Connection [2] [3, 2] "b"
           , Connection [3] [1] "e"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""
    c = StateDiagram [d, e] 3 ""
        [ Connection [1] [2] "c"
        , Connection [2] [1] "d"]
        [1]
      where
        d = InnerMostState 1 "C" ""
        e = InnerMostState 2 "D" ""

{- Accepted by Alloy directly -}
slide278 :: UMLStateDiagram String Int
slide278 = umlStateDiagram $
           StateDiagram [a, b, c, d] 8 ""
           [ Connection [1] [3] "a"
           , Connection [1] [2] "f"
           , Connection [2] [4] "b"
           , Connection [3] [4] "c"
           , Connection [3] [1] "e"
           , Connection [4] [3] "d"
           , Connection [4] [1] "e"]
           [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""
    c = InnerMostState 3 "C" ""
    d = InnerMostState 4 "D" ""

{- Accepted by Alloy directly -}
slide279 :: UMLStateDiagram String Int
slide279 = umlStateDiagram $
           StateDiagram [a, b, c, d, l] 16 ""
           [ Connection [1] [2] "a"
           , Connection [2, 1, 2] [4] "h"
           , Connection [2, 1, 3] [3] ""
           , Connection [2, 2, 2] [3] ""
           , Connection [3] [4] "g"
           , Connection [4] [5] "" ]
           [1]
  where
    a = InnerMostState 1 "A" ""
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, i] 1 ""
            [ Connection [1] [2] "b"
            , Connection [2] [3] "c"]
            [1]
          where
            g = InnerMostState 1 "B" ""
            h = InnerMostState 2 "C" ""
            i = InnerMostState 3 "D" ""
        f = StateDiagram [j, k] 2 ""
            [ Connection [1] [2] "e" ]
            [1]
          where
            j = InnerMostState 1 "E" ""
            k = InnerMostState 2 "F" ""
    c = ForkOrJoin 3
    d = InnerMostState 4 "G" ""
    l = EndState 5

{- Accepted by Alloy directly -}
slide280 :: UMLStateDiagram String Int
slide280 = umlStateDiagram $
           StateDiagram [a, b, c, d, e, f, g, h] 12 ""
           [ Connection [1] [2] "a"
           , Connection [2] [3] "b"
           , Connection [2] [5] "e"
           , Connection [3] [4] "c"
           , Connection [3] [6] "e"
           , Connection [4] [7] "e"
           , Connection [5] [6] "b"
           , Connection [6] [7] "c"
           , Connection [6] [8] "h"
           , Connection [7] [8] "g"
           , Connection [3] [8] "h" ]
           [1]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "(B, E)" ""
    c = InnerMostState 3 "(C, E)" ""
    d = InnerMostState 4 "(D, E)" ""
    e = InnerMostState 5 "(B, F)" ""
    f = InnerMostState 6 "(C, F)" ""
    g = InnerMostState 7 "(D, F)" ""
    h = InnerMostState 8 "G" ""

{- Accepted by Alloy directly -}
slide281 :: UMLStateDiagram String Int
slide281 = umlStateDiagram $
           StateDiagram [a, b, c, d] 14 ""
           [ Connection [1] [2] "a"
           , Connection [2] [1] "f"
           , Connection [2] [3, 0] "b"
           , Connection [3] [1] "e"
           , Connection [4] [3] "x"]
           [4]
  where
    a = InnerMostState 1 "A" ""
    b = InnerMostState 2 "B" ""
    c = StateDiagram [e, f, g] 3 ""
        [ Connection [1] [2] "c"
        , Connection [2] [1] "d"
        , Connection [0] [1] "" ]
        [1]
      where
        e = InnerMostState 1 "C" ""
        f = InnerMostState 2 "D" ""
        g = History 0 Shallow
    d = InnerMostState 4 "X" ""

{- Accepted by Alloy directly -}
slide283 :: UMLStateDiagram String Int
slide283 = umlStateDiagram $
           StateDiagram [a, b, c, d, e, f, g] 12 ""
           [ Connection [1] [2] "a"
           , Connection [2] [1] "f"
           , Connection [3] [4] "a"
           , Connection [4] [3] "f"
           , Connection [2] [5] "b"
           , Connection [4] [6] "b"
           , Connection [5] [6] "c"
           , Connection [6] [5] "d"
           , Connection [6] [3] "e"
           , Connection [5] [1] "e"
           , Connection [7] [5] "x"]
           [7]
  where
    a = InnerMostState 1 "A(C)" ""
    b = InnerMostState 2 "B(C)" ""
    c = InnerMostState 3 "A(D)" ""
    d = InnerMostState 4 "B(D)" ""
    e = InnerMostState 5 "C" ""
    f = InnerMostState 6 "D" ""
    g = InnerMostState 7 "X()" ""

{- Accepted by Alloy directly -}
task26a :: UMLStateDiagram String Int
task26a = umlStateDiagram $ StateDiagram [a, b, c, d] 23 ""
          [ Connection [1, 1, 3] [2] "e"
          , Connection [1, 2, 2] [2] "e"
          , Connection [2] [3] ""
          , Connection [3] [4] "c"
          , Connection [3] [4, 2] "d"
          , Connection [4, 2] [1, 2, 2] "a"
          , Connection [4, 1] [1] "a" ]
          [1, 1, 2]
  where
    a = CombineDiagram [e, f] 1
      where
        e = StateDiagram [i, j, k] 1 ""
            [ Connection [1] [2] "a"
            , Connection [2] [3] "b"
            , Connection [3] [1] "a"] [1]
          where
            i = InnerMostState 1 "A" ""
            j = InnerMostState 2 "B" ""
            k = InnerMostState 3 "C" ""
        f = StateDiagram [l, m] 2 ""
            [ Connection [1] [2] "b"
            , Connection [2] [1] "c"]
            [1]
          where
            l = InnerMostState 1 "D" ""
            m = InnerMostState 2 "E" ""
    b = ForkOrJoin 2
    c = InnerMostState 3 "F" ""
    d = StateDiagram [g, h] 4 "" [Connection [2] [1] "d"] [2]
      where
        g = InnerMostState 1 "G" ""
        h = InnerMostState 2 "H" ""

{- Accepted by Alloy directly -}
task26b :: UMLStateDiagram String Int
task26b = umlStateDiagram $
          StateDiagram [a, b, c, d, e, f, g, h, i] 17 ""
          [Connection [1] [2] "a"
          , Connection [1] [4] "b"
          , Connection [2] [6] "b"
          , Connection [3] [6] "b"
          , Connection [3] [1] "a"
          , Connection [4] [1] "b"
          , Connection [4] [5] "a"
          , Connection [5] [3] "b"
          , Connection [6] [3] "b"
          , Connection [6] [4] "a"
          , Connection [6] [7] "c"
          , Connection [7] [9] "c"
          , Connection [7] [9] "d"
          , Connection [9] [8] "d"
          , Connection [8] [1] "a"
          , Connection [9] [4] "a"]
          [2]
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

{- Accepted by Alloy directly -}
task27 :: UMLStateDiagram String Int
task27 = umlStateDiagram $
         StateDiagram [a, b] 12 ""
         [ Connection [1] [2, 0] "F"
         , Connection [2] [1] "F" ]
         [1]
  where
    a = InnerMostState 1 "CD" ""
    b = StateDiagram [c, d, e, f] 2 "Radio"
        [ Connection [0] [1] ""
        , Connection [1] [2] "+"
        , Connection [2] [3] "+"
        , Connection [3] [2] "-"
        , Connection [2] [1] "-"] []
      where
        c = InnerMostState 1 "1" ""
        d = InnerMostState 2 "2" ""
        e = InnerMostState 3 "3" ""
        f = History 0 Shallow

-- task28 is the same as task26a

{- Accepted by Alloy directly -}
task85 :: UMLStateDiagram String Int
task85 = umlStateDiagram $
         StateDiagram [ForkOrJoin 1, a] 35 ""
         [ Connection [1] [2, 1, 1, 1, 1] ""
         , Connection [1] [2, 1, 1, 2, 1] ""]
         [1]
  where
    a = StateDiagram [b, c] 2 "A"
        [ Connection [1, 1, 1, 1] [2] "i"
        , Connection [1, 3] [2] ""
        , Connection [1, 2, 2] [2, 2] "h"
        , Connection [2, 2] [1, 1, 2, 3] "e"
        , Connection [2, 1] [1, 0] "f"] []
      where
        b = StateDiagram [g, h, i, j] 1 "B"
            [ Connection [1, 1, 2] [3] "c"
            , Connection [1, 2, 2] [3] "c"
            , Connection [0] [2] ""] []
          where
            g = CombineDiagram [k, l] 1
              where
                k = StateDiagram [ InnerMostState 1 "1" ""
                                 , InnerMostState 2 "2" ""] 1 "C"
                    [ Connection [1] [2] "a"
                    , Connection [2] [1] "b"]
                    [1]
                l = StateDiagram [ InnerMostState 1 "3" ""
                                 , InnerMostState 2 "4" ""
                                 , InnerMostState 3 "5" ""] 2 ""
                    [ Connection [1] [2] "b"
                    , Connection [2] [3] "b"
                    , Connection [3] [1] "b"] [1]
            h = StateDiagram [m, n] 2 "D"
                [ Connection [1] [2] "g"
                , Connection [2] [1] "g"] [1]
              where
                m = InnerMostState 1 "9" ""
                n = InnerMostState 2 "10" ""
            i = ForkOrJoin 3
            j = History 0 Shallow
        c = StateDiagram [d, e, f] 2 "E"
            [ Connection [1] [3] "d"
            , Connection [3] [2] "d"
            , Connection [2] [1] "d"] [3]
          where
            d = InnerMostState 1 "6" ""
            e = InnerMostState 2 "7" ""
            f = InnerMostState 3 "8" ""

{- Accepted by Alloy directly -}
task88 :: UMLStateDiagram String Int
task88 = umlStateDiagram $
         StateDiagram [a, b, c] 22 ""
         [ Connection [1] [3] "k"
         , Connection [2] [3] "k"
         , Connection [3, 2] [2] "h"
         , Connection [2] [1, 2, 2] "h"] [3]
  where
    a = CombineDiagram [d, e] 1
      where
        d = StateDiagram [h, i] 1 ""
            [ Connection [1] [2] "a"
            , Connection [2] [1] "a"]
            [1]
          where
            h = InnerMostState 1 "A" ""
            i = InnerMostState 2 "B" ""
        e = StateDiagram [j, k] 2 ""
            [ Connection [1] [2] "b"
            , Connection [2] [1] "b"] [1]
          where
            j = InnerMostState 1 "C" ""
            k = InnerMostState 2 "D" ""
    b = InnerMostState 2 "E" ""
    c = StateDiagram [f, g] 3 ""
        [ Connection [1] [2] "a"
        , Connection [2] [1] "a"]
        [1]
      where
        f = InnerMostState 1 "F" ""
        g = InnerMostState 2 "G" ""

{- After some changes, accepted by Alloy -}
test2 :: UMLStateDiagram String Int
test2 = umlStateDiagram $
        StateDiagram [a, b] 7 ""
        [ Connection [1] [2, 1] "a"] [1]
  where
    a = InnerMostState 1 "A" ""
    b = StateDiagram [c, d] 2 "B"
        [Connection [1] [2] "b"] [1]
      where
        c = InnerMostState 1 "C" ""
        d = StateDiagram [e, f] 2 "D"
            [Connection [1] [2] "c"] [1]
          where
            e = InnerMostState 1 "E" ""
            f = InnerMostState 2 "F" ""

{- Accepted by Alloy directly -}
test4 :: UMLStateDiagram String Int
test4 = umlStateDiagram $
        StateDiagram [a] 35 "" [] [1, 1, 1]
  where
    a = StateDiagram [b, c] 1 "A"
        [ Connection [1, 2, 1, 1] [2] "i"
        , Connection [1, 3] [2] ""
        , Connection [2, 2] [1, 2, 2, 3] "e"
        , Connection [1, 5, 2] [2, 2] "h"
        , Connection [2, 1] [1, 4] "f"]
        []
    b = StateDiagram [b1, b2, b3, b4, b5] 1 "B"
        [ Connection [4] [5] ""
        , Connection [1] [2, 1, 1] ""
        , Connection [1] [2, 2, 3] ""
        , Connection [2, 1, 2] [3] "c"
        , Connection [2, 2, 2] [3] "c"]
        []
    c = StateDiagram [c1, c2, c3] 2 "E"
        [ Connection [1] [3] "d"
        , Connection [3] [2] "d"
        , Connection [2] [1] "d"] [3]
    c1 = InnerMostState 1 "6" ""
    c2 = InnerMostState 2 "7" ""
    c3 = InnerMostState 3 "8" ""
    b1 = ForkOrJoin 1
    b2 = CombineDiagram [d1, d2] 2
    b3 = ForkOrJoin 3
    b4 = History 4 Shallow
    b5 = StateDiagram [e1, e2] 5 "D"
         [ Connection [1] [2] "g"
         , Connection [2] [1] "g"] [1]
    e1 = InnerMostState 1 "9" ""
    e2 = InnerMostState 2 "10" ""
    d1 = StateDiagram [f1, f2] 1 "C"
         [ Connection [1] [2] "a"
         , Connection [2] [1] "a"]
         [1]
    d2 = StateDiagram [g1, g2, g3] 2 ""
         [ Connection [1] [2] "b"
         , Connection [2] [3] "b"
         , Connection [3] [1] "b"]
         [1]
    f1 = InnerMostState 1 "1" ""
    f2 = InnerMostState 2 "2" ""
    g1 = InnerMostState 1 "3" ""
    g2 = InnerMostState 2 "4" ""
    g3 = InnerMostState 3 "5" ""

noHistoryTest4 :: UMLStateDiagram String Int
noHistoryTest4 = umlStateDiagram $
        StateDiagram [a] 35 "" [] [1, 1, 1]
  where
    a = StateDiagram [b, c] 1 "A"
        [ Connection [1, 2, 1, 1] [2] "i"
        , Connection [1, 3] [2] ""
        , Connection [2, 2] [1, 2, 2, 3] "e"
        , Connection [1, 5, 2] [2, 2] "h" ]
        []
    b = StateDiagram [b1, b2, b3, b5] 1 "B"
        [ Connection [1] [2, 1, 1] ""
        , Connection [1] [2, 2, 3] ""
        , Connection [2, 1, 2] [3] "c"
        , Connection [2, 2, 2] [3] "c"]
        []
    c = StateDiagram [c1, c2, c3] 2 "E"
        [ Connection [1] [3] "d"
        , Connection [3] [2] "d"
        , Connection [2] [1] "d"] [3]
    c1 = InnerMostState 1 "6" ""
    c2 = InnerMostState 2 "7" ""
    c3 = InnerMostState 3 "8" ""
    b1 = ForkOrJoin 1
    b2 = CombineDiagram [d1, d2] 2
    b3 = ForkOrJoin 3
    b5 = StateDiagram [e1, e2] 5 "D"
         [ Connection [1] [2] "g"
         , Connection [2] [1] "g"] [1]
    e1 = InnerMostState 1 "9" ""
    e2 = InnerMostState 2 "10" ""
    d1 = StateDiagram [f1, f2] 1 "C"
         [ Connection [1] [2] "a"
         , Connection [2] [1] "a"]
         [1]
    d2 = StateDiagram [g1, g2, g3] 2 ""
         [ Connection [1] [2] "b"
         , Connection [2] [3] "b"
         , Connection [3] [1] "b"]
         [1]
    f1 = InnerMostState 1 "1" ""
    f2 = InnerMostState 2 "2" ""
    g1 = InnerMostState 1 "3" ""
    g2 = InnerMostState 2 "4" ""
    g3 = InnerMostState 3 "5" ""

testFlatConReg1 :: UMLStateDiagram String Int
testFlatConReg1 = let
                  isA = InnerMostState 1 "A" ""
                  isB = InnerMostState 2 "B" ""
                  isC = InnerMostState 3 "C" ""
                  cd1r1 = StateDiagram [isA, isB, isC] 1 ""
                         [ Connection [1] [2] "a"
                         , Connection [2] [3] "b"
                         , Connection [3] [1] "a"]
                         [1]
                  in
                  umlStateDiagram cd1r1

testFlatConReg2 :: UMLStateDiagram String Int
testFlatConReg2 = let
                  isD = InnerMostState 1 "D" ""
                  isE = InnerMostState 2 "E" ""
                  cd1r2 = StateDiagram [isD, isE] 2 ""
                          [ Connection [1] [2] "b"
                          , Connection [2] [1] "b"]
                          [1]
                  in
                  umlStateDiagram cd1r2

flatCase2 :: UMLStateDiagram String Int
flatCase2 = let
                 isH = InnerMostState 2 "H" ""
                 isG = InnerMostState 1 "G" ""
                 isA = InnerMostState 1 "A" ""
                 isB = InnerMostState 2 "B" ""
                 isC = InnerMostState 3 "C" ""
                 isD = InnerMostState 1 "D" ""
                 isE = InnerMostState 2 "E" ""
                 isF = InnerMostState 2 "F" ""
                 jn1 = ForkOrJoin 5
                 cd1 = CombineDiagram [cd1r1, cd1r2] 1
                 cd1r1 = StateDiagram [isA, isB, isC] 1 ""
                        [ Connection [1] [2] "a"
                        , Connection [2] [3] "b"
                        , Connection [3] [1] "a"]
                        [1]
                 cd1r2 = StateDiagram [isD, isE] 2 ""
                         [ Connection [1] [2] "b"
                         , Connection [2] [1] "b"]
                         [1]
                 sd2 = StateDiagram [isH, isG] 4 ""
                      [ Connection [2] [1] "d"]
                      [2]
                 in
                   umlStateDiagram $ StateDiagram [cd1, isF, sd2, jn1] 22 ""
                     [ Connection [2] [4] "c"
                     , Connection [2] [4,2] "d"
                     , Connection [4, 2] [1, 2, 2] "a"
                     , Connection [4, 1] [1] "a"
                     , Connection [1,1,3] [5] ""
                     , Connection [1,2,2] [5] ""
                     , Connection [5] [2] "c" ]
                     [1, 1, 2]

testFlat1Combine :: [[StateDiagram String Int [Connection Int]]]
testFlat1Combine = let
                   isD = InnerMostState 1 "D" ""
                   isE = InnerMostState 2 "E" ""
                   isA = InnerMostState 1 "A" ""
                   isB = InnerMostState 2 "B" ""
                   isC = InnerMostState 3 "C" ""
                   in
                     [[isA, isB, isC],[isD, isE]]

flatCase1 :: UMLStateDiagram String Int
flatCase1 = let
            isA = InnerMostState 1 "A" ""
            isB = InnerMostState 2 "B" ""
            isC = InnerMostState 3 "C" ""
            isG = InnerMostState 1 "G" ""
            isH = InnerMostState 2 "H" ""
            sd1 = StateDiagram [isG,isH] 4 "P"
                  [ Connection [2] [1] "b" ]
                  [2]
            in
              umlStateDiagram $ StateDiagram [isA,isB,isC,sd1] 0 ""
                [ Connection [1] [4] "a"
                , Connection [4,1] [2] "c"
                , Connection [2] [3] "d"
                , Connection [3] [4,1] "e" ]
                [4]

flatCase7 :: UMLStateDiagram String Int
flatCase7
  = let
    isA = InnerMostState 1 "A" ""
    isB = InnerMostState 2 "B" ""
    isC = InnerMostState 3 "C" ""
    isG = InnerMostState 1 "G" ""
    isH = InnerMostState 2 "H" ""
    sd1 = StateDiagram [isG,isH] 4 "P"
          [ Connection [2] [1] "b" ]
          [2]
    isE = InnerMostState 1 "E" ""
    isF = InnerMostState 2 "F" ""
    sd2 = StateDiagram [isE,isF] 5 "D"
          [ Connection [1] [2] "h" ]
          [1]
    in
    umlStateDiagram $ StateDiagram [isA,isB,isC,sd1,sd2] 0 ""
      [ Connection [1] [4] "a"
      , Connection [4,1] [2] "c"
      , Connection [2] [3] "d"
      , Connection [3] [4,1] "e"
      , Connection [3] [5] "f"
      , Connection [5,2] [1] "g" ]
      [4]


flatCase3 :: UMLStateDiagram String Int
flatCase3 = let
            isA = InnerMostState 1 "A" ""
            isB = InnerMostState 2 "B" ""
            isC = InnerMostState 3 "C" ""
            isG = InnerMostState 1 "G" ""
            isH = InnerMostState 2 "H" ""
            isI = InnerMostState 1 "I" ""
            isJ = InnerMostState 2 "J" ""
            sd1 = StateDiagram [isG,isH] 4 "P1"
                  [ Connection [2] [1] "b" ]
                  [2]
            sd2 = StateDiagram [isI, isJ] 5 "P2"
                  [ Connection [1] [2] "i" ]
                  [2]
            in
              umlStateDiagram $ StateDiagram [isA,isB,isC,sd1,sd2] 0 ""
                [ Connection [1] [4] "a"
                , Connection [4,1] [2] "c"
                , Connection [2] [3] "d"
                , Connection [3] [4,1] "e" ]
                [4]

positiveExamples :: [(String, UMLStateDiagram String Int)]
positiveExamples =
        [ ("verySmall", verySmall)
        , ("picture1", picture1)
        , ("picture2", picture2)
        , ("picture3", picture3)
        , ("picture4", picture4)
        , ("slide246", slide246)
        , ("slide253", slide253)
        , ("slide257", slide257)
        , ("slide267a", slide267a)
        , ("slide267b", slide267b)
        , ("slide271", slide271)
        , ("slide273", slide273)
        , ("slide275", slide275)
        , ("slide277", slide277)
        , ("slide278", slide278)
        , ("slide279", slide279)
        , ("slide280", slide280)
        , ("slide281", slide281)
        , ("slide283", slide283)
        , ("task26a", task26a)
        , ("task26b", task26b)
        , ("task27", task27)
        , ("task85", task85)
        , ("task88", task88)
        , ("test2", test2)
        , ("test4", test4)
        ]

{- because the integrated PlantUML front end renderer
   doesn't support concurrent regions yet... -}
posPlantUMLExamples :: [(String, UMLStateDiagram String Int)]
posPlantUMLExamples = [ ("verySmall", verySmall)
                      , ("picture2", picture2)
                      , ("picture4", picture4)
                      , ("slide246", slide246)
                      , ("slide253", slide253)
                      , ("slide257", slide257)
                      , ("slide267a", slide267a)
                      , ("slide267b", slide267b)
                      , ("slide271", slide271)
                      , ("slide273", slide273)
                      , ("slide277", slide277)
                      , ("slide278", slide278)
                      , ("slide280", slide280)
                      , ("slide281", slide281)
                      , ("slide283", slide283)
                      , ("task26b", task26b)
                      , ("task27", task27)
                      , ("test2", test2)
        ]
