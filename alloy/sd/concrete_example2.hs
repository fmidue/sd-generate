module Solution where

data Connection = Connection [Integer] [Integer] String
data HistoryType = Shallow | Deep

type UMLStateDiagram = StateDiagram [Connection]
data StateDiagram a =
  StateDiagram { substate :: [StateDiagram a],
                 label :: Int,
                 name :: String,
                 connection :: a,
                 startState :: [Int]
               }
  | CombineDiagram { substate :: [StateDiagram a],
                   label :: Int
                 }
  | EndState {
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
                 }

picture2 :: UMLStateDiagram
picture2 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2, 1, 1] "", Connection [1] [2, 2, 1] "",
  Connection [2, 1, 3] [3] "", Connection [2, 2, 2] [3] "", Connection [3] [4] ""] [1]
  where
    a = Joint 1
    b = CombineDiagram [e, f] 2
      where
        e = StateDiagram [g, h, i] 1 "4" [Connection [1] [2, 1, 1] "", Connection [1] [2, 2, 1] "",
          Connection [2, 1, 2] [3] "-2", Connection [2, 2, 1] [3] "-2"] []
          where
            g = Joint 1
            h = CombineDiagram [l, m] 2
              where
                l = StateDiagram [n, o] 1 "6" [Connection [1] [2] "-1"] []
                  where
                    n = InnerMostState 1 "1" ""
                    o = InnerMostState 2 "2" ""
                m = StateDiagram [p] 2 "7" [] []
                  where
                    p = InnerMostState 1 "1" ""
            i = Joint 3
        f = StateDiagram [j, k] 2 "5" [Connection [1] [2] "-1"] []
          where
            j = InnerMostState 1 "3" ""
            k = InnerMostState 2 "2" ""
    c = Joint 3
    d = EndState 4

{-
  Some not-so-sophisticated ideas:
  I feel that translating Alloy instance text into a Haskell version more difficult, because it dosen't have something like address in Alloy version.
  Without something like address, translating Flows in Alloy into Connection in Hakell is the main difficulty.
  I think we need to translate a basic structure without flows at first and to set labels, then we can get all connections according to Flows in Alloy and these labels.
  For example:
  Flows.from = NormalState1 -> a
  Flows.to = NormalState2 -> b
  Flows.label = TriggerNames -> c
  It can be translated into Connection [a] [b] "c" at first;
  After all labels are set, then all nodes have a label, such as a maps 1, b maps 2
  then translate Connection [a] [b] "c" into Connection [1] [2];
  of course, it is involved with relative address, which is easy to solve.
  
picture2 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2, 1, 1] "", Connection [1] [2, 2, 1] ""
  Connection [2, 1, 3] [3] "", Connection [2, 2, 2] [3] "", Connection [3] [4] ""] [1]
  
This is the outermost level, it is a default level when translating, [a, b, c, d] = Nodes - allContainedNodes - StartNodes;
I think labels can be set in a sequence;
"" is default
[Connectiong]: Flows.from -> the first parameter, Flows.to -> the second parameter, Flows.label -> the third parameter, but there are no labels of nodes in Alloy, so we need to translate them after all labels are set completely.
[1]: If a node in this level, and a Flows whose Flows.from in StartNodes and Flows.to is the node, then this list have the relative address of the node. (Setting labels is also the first step)

A ForkNodes or JoinNodes -> "Joint"
An EndNodes -> "EndState"
A ShallowHistoryNodes -> History "*" Shallow
A DeepHistoryNofrd -> History * Deep
A NormalState -> InnerMostState * "" "", the second parameter is a ComponentsName
RegionsStates -> CombineDiagram
RegionsStates.contains -> the first parameter of CombineDiagram
Regions or HierarchicalStates -> StateDiagrams
Regions.contains or HierarchicalStates.contains -> the first parameter of StateDiagrams
Regions.name or HierarchicalStates.name -> the third parameter of StateDiagrams

For example:
e = StateDiagram [g, h, i] 1 "4" [Connection [1] [2, 1, 1] "", Connection [1] [2, 2, 1] "" 
  Connection [2, 1, 2] [3] "-2", Connection [2, 2, 1] [3] "-2"] []
  
"StateDiagram" is corresponding to HerarchicalStates or Regions, but it is under a "where" of CombineDiagram, so it is a Region.
1 is a label, which should be set in sequence
"4" is Region.name
As for Connection, it is same as above.

EmpptyTrigger -> ""
As for CommponetsNames and TriggerNames
I think setting a injective mapping from CommponetsNames and TriggerNames to a set {a, b, c, d ...} is enough
-}