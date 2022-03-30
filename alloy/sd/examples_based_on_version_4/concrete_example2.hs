module Solution where

{-
---INSTANCE---
loop=0
end=0
integers={
-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7}
univ={
-1, -2, -3, -4, -5, -6, -7, -8, 0, 1, 2, 3, 4, 5, 6, 7, C1$0, C2$0, E1$0, F1$0, F2$0, Flow1$0, Flow10$0, Flow11$0, Flow12$0, Flow2$0, Flow3$0, Flow4$0, Flow5$0, Flow6$0, Flow7$0, Flow8$0, Flow9$0, J1$0, J2$0, N1$0, N2$0, N3$0, N4$0, N5$0, Name1$0, Name2$0, Name3$0, Name4$0, Name5$0, Name6$0, Name7$0, R1$0, R2$0, R3$0, R4$0, S1$0, T1$0, T2$0, uml_state_diagram/components/EmptyTrigger$0, uml_state_diagram/components/ProtoFlows$0, uml_state_diagram/components/ProtoFlows$1, uml_state_diagram/components/ProtoFlows$2, uml_state_diagram/components/ProtoFlows$3, uml_state_diagram/components/ProtoFlows$4, uml_state_diagram/components/ProtoFlows$5, uml_state_diagram/components/ProtoFlows$6, uml_state_diagram/components/ProtoFlows$7, uml_state_diagram/components/ProtoFlows$8}
Int={
-1, -2, -3, -4, -5, -6, -7, -8, 0, 1, 2, 3, 4, 5, 6, 7}
seq/Int={0, 1, 2, 3, 4, 5, 6}
String={}
none={}
uml_state_diagram/components/ProtoFlows={Flow1$0, Flow10$0, Flow11$0, Flow12$0, Flow2$0, Flow3$0, Flow4$0, Flow5$0, Flow6$0, Flow7$0, Flow8$0, Flow9$0, uml_state_diagram/components/ProtoFlows$0, uml_state_diagram/components/ProtoFlows$1, uml_state_diagram/components/ProtoFlows$2, uml_state_diagram/components/ProtoFlows$3, uml_state_diagram/components/ProtoFlows$4, uml_state_diagram/components/ProtoFlows$5, uml_state_diagram/components/ProtoFlows$6, uml_state_diagram/components/ProtoFlows$7, uml_state_diagram/components/ProtoFlows$8}
uml_state_diagram/components/ProtoFlows<:from={Flow1$0->S1$0, Flow10$0->N5$0, Flow11$0->J1$0, Flow12$0->J2$0, Flow2$0->F1$0, Flow3$0->F1$0, Flow4$0->F2$0, Flow5$0->F2$0, Flow6$0->N1$0, Flow7$0->N2$0, Flow8$0->N3$0, Flow9$0->N4$0, uml_state_diagram/components/ProtoFlows$0->S1$0, uml_state_diagram/components/ProtoFlows$1->S1$0, uml_state_diagram/components/ProtoFlows$2->N2$0, uml_state_diagram/components/ProtoFlows$3->N4$0, uml_state_diagram/components/ProtoFlows$4->N5$0, uml_state_diagram/components/ProtoFlows$5->N4$0, uml_state_diagram/components/ProtoFlows$6->N2$0, uml_state_diagram/components/ProtoFlows$7->S1$0, uml_state_diagram/components/ProtoFlows$8->S1$0}
uml_state_diagram/components/ProtoFlows<:to={Flow1$0->F1$0, Flow10$0->J2$0, Flow11$0->J2$0, Flow12$0->E1$0, Flow2$0->F2$0, Flow3$0->N3$0, Flow4$0->N1$0, Flow5$0->N4$0, Flow6$0->N2$0, Flow7$0->J1$0, Flow8$0->N5$0, Flow9$0->J1$0, uml_state_diagram/components/ProtoFlows$0->N4$0, uml_state_diagram/components/ProtoFlows$1->N1$0, uml_state_diagram/components/ProtoFlows$2->E1$0, uml_state_diagram/components/ProtoFlows$3->E1$0, uml_state_diagram/components/ProtoFlows$4->E1$0, uml_state_diagram/components/ProtoFlows$5->J2$0, uml_state_diagram/components/ProtoFlows$6->J2$0, uml_state_diagram/components/ProtoFlows$7->N3$0, uml_state_diagram/components/ProtoFlows$8->F2$0}
uml_state_diagram/components/ProtoFlows<:derived={Flow1$0->uml_state_diagram/components/ProtoFlows$7, Flow1$0->uml_state_diagram/components/ProtoFlows$8, Flow10$0->uml_state_diagram/components/ProtoFlows$4, Flow7$0->uml_state_diagram/components/ProtoFlows$6, Flow9$0->uml_state_diagram/components/ProtoFlows$5, uml_state_diagram/components/ProtoFlows$5->uml_state_diagram/components/ProtoFlows$3, uml_state_diagram/components/ProtoFlows$6->uml_state_diagram/components/ProtoFlows$2, uml_state_diagram/components/ProtoFlows$8->uml_state_diagram/components/ProtoFlows$0, uml_state_diagram/components/ProtoFlows$8->uml_state_diagram/components/ProtoFlows$1}
uml_state_diagram/components/Flows={Flow1$0, Flow10$0, Flow11$0, Flow12$0, Flow2$0, Flow3$0, Flow4$0, Flow5$0, Flow6$0, Flow7$0, Flow8$0, Flow9$0}
uml_state_diagram/components/Flows<:label={Flow1$0->uml_state_diagram/components/EmptyTrigger$0, Flow10$0->uml_state_diagram/components/EmptyTrigger$0, Flow11$0->uml_state_diagram/components/EmptyTrigger$0, Flow12$0->uml_state_diagram/components/EmptyTrigger$0, Flow2$0->uml_state_diagram/components/EmptyTrigger$0, Flow3$0->uml_state_diagram/components/EmptyTrigger$0, Flow4$0->uml_state_diagram/components/EmptyTrigger$0, Flow5$0->uml_state_diagram/components/EmptyTrigger$0, Flow6$0->T1$0, Flow7$0->T2$0, Flow8$0->T1$0, Flow9$0->T2$0}
this/Flow1={Flow1$0}
this/Flow2={Flow2$0}
this/Flow3={Flow3$0}
this/Flow4={Flow4$0}
this/Flow5={Flow5$0}
this/Flow6={Flow6$0}
this/Flow7={Flow7$0}
this/Flow8={Flow8$0}
this/Flow9={Flow9$0}
this/Flow10={Flow10$0}
this/Flow11={Flow11$0}
this/Flow12={Flow12$0}
uml_state_diagram/components/Nodes={C1$0, C2$0, E1$0, F1$0, F2$0, J1$0, J2$0, N1$0, N2$0, N3$0, N4$0, N5$0, S1$0}
uml_state_diagram/components/StartNodes={S1$0}
uml_state_diagram/components/StartNodes<:flag={S1$0->0}
this/S1={S1$0}
uml_state_diagram/components/States={C1$0, C2$0, N1$0, N2$0, N3$0, N4$0, N5$0}
uml_state_diagram/components/States<:name={N1$0->Name1$0, N2$0->Name2$0, N3$0->Name3$0, N4$0->Name1$0, N5$0->Name2$0}
uml_state_diagram/components/NormalStates={N1$0, N2$0, N3$0, N4$0, N5$0}
this/N1={N1$0}
this/N2={N2$0}
this/N3={N3$0}
this/N4={N4$0}
this/N5={N5$0}
uml_state_diagram/components/CompositeStates={C1$0, C2$0}
uml_state_diagram/components/RegionsStates={C1$0, C2$0}
uml_state_diagram/components/RegionsStates<:contains={C1$0->R1$0, C1$0->R2$0, C2$0->R3$0, C2$0->R4$0}
this/C1={C1$0}
this/C2={C2$0}
uml_state_diagram/components/HierarchicalStates={}
uml_state_diagram/components/HierarchicalStates<:contains={}
uml_state_diagram/components/ForkNodes={F1$0, F2$0}
this/F1={F1$0}
this/F2={F2$0}
uml_state_diagram/components/JoinNodes={J1$0, J2$0}
this/J1={J1$0}
this/J2={J2$0}
uml_state_diagram/components/EndNodes={E1$0}
this/E1={E1$0}
uml_state_diagram/components/HistoryNodes={}
uml_state_diagram/components/ShallowHistoryNodes={}
uml_state_diagram/components/DeepHistoryNodes={}
uml_state_diagram/components/ComponentNames={Name1$0, Name2$0, Name3$0, Name4$0, Name5$0, Name6$0, Name7$0}
this/Name1={Name1$0}
this/Name2={Name2$0}
this/Name3={Name3$0}
this/Name4={Name4$0}
this/Name5={Name5$0}
this/Name6={Name6$0}
this/Name7={Name7$0}
uml_state_diagram/components/Triggers={T1$0, T2$0, uml_state_diagram/components/EmptyTrigger$0}
uml_state_diagram/components/TriggerNames={T1$0, T2$0}
this/T1={T1$0}
this/T2={T2$0}
uml_state_diagram/components/EmptyTrigger={uml_state_diagram/components/EmptyTrigger$0}
uml_state_diagram/components/Regions={R1$0, R2$0, R3$0, R4$0}
uml_state_diagram/components/Regions<:name={R1$0->Name4$0, R2$0->Name5$0, R3$0->Name6$0, R4$0->Name7$0}
uml_state_diagram/components/Regions<:contains={R1$0->C2$0, R1$0->F2$0, R1$0->J1$0, R2$0->N3$0, R2$0->N5$0, R3$0->N1$0, R3$0->N2$0, R4$0->N4$0}
this/R1={R1$0}
this/R2={R2$0}
this/R3={R3$0}
this/R4={R4$0}
-}

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
  | EndState { label :: Int
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
picture2 = StateDiagram [a, b, c, d] 1 "" [Connection [1] [2, 1, 1] "", Connection [1] [2, 2, 1] "", -- default outermost level, F1$0 -> a, C1$0 -> b, J2$0 -> c, E1$0 ->d
  Connection [2, 1, 3] [3] "", Connection [2, 2, 2] [3] "", Connection [3] [4] ""] [1] -- [Flow2$0 (this.)from to label, Flow3$0 from to label, Flow10$0 from to label, Flow11$0 from to label, Flow12$0 from to label] -> [Connection] here, Flow1$0 from(S1$0) to label -> [1]
  where
    a = Joint 1 -- F1$0 -> a
    b = CombineDiagram [e, f] 2 -- C1$0 -> b, R1$0 -> e, R2$0 -> f, C1$0 <: contains -> CombineDiagram [e, f]
      where
        e = StateDiagram [g, h, i] 1 "4" [Connection [1] [2, 1, 1] "", Connection [1] [2, 2, 1] "", -- R1$0 -> e, F2$0 -> g, C2$0 -> h, J1$0 -> i, Name4$0 -> "4", R1$0 <: name -> "4"
          Connection [2, 1, 2] [3] "-2", Connection [2, 2, 1] [3] "-2"] [] -- [Flow4$0 from to label, Flow5$0 from to label, Flow7$0 from to label, Flow9$0 from to label] -> [Connection] here
          where
            g = Joint 1 -- F2$0 -> g
            h = CombineDiagram [l, m] 2 -- C2$0 -> h, R3$0 -> l, R4$0 -> m, C2$0 <: contains -> CombineDiagram [l, m]
              where
                l = StateDiagram [n, o] 1 "6" [Connection [1] [2] "-1"] [] -- R3$0 -> l, N1$0 -> n, N2$0 -> o, R3$0 <: contains -> StateDiagram [n, o], Name6$0 -> "6", R3$0 <: name -> "6", [Flow6$0 from label to] -> [Connection] here
                  where
                    n = InnerMostState 1 "1" ""
                    o = InnerMostState 2 "2" ""
                m = StateDiagram [p] 2 "7" [] [] -- R4$0 -> m, N4$0 -> p, R4$0 <: contains -> StateDiagram [p], Name7$0 -> "7", R4$0 <: name = "7"
                  where
                    p = InnerMostState 1 "1" "" -- N4$0 -> p， Name1$0 -> “1”， N4$0 <: name = "1"
            i = Joint 3 -- J1$0 -> i
        f = StateDiagram [j, k] 2 "5" [Connection [1] [2] "-1"] [] -- R2$0 -> f，N3$0 -> j， N5$0 -> k， R2$0 <: contains -> StateDiagram [j, k], Name5$0 -> "5", R2$0 <: name -> "5", [Flow8$0 from, to, label] -> [Connection] here
          where
            j = InnerMostState 1 "3" "" -- N3$0 -> j, Name3$0 -> "3", N3$0 <: name -> "3"
            k = InnerMostState 2 "2" "" -- N5$0 -> k, Name2$0 -> "2", N5$0 <: name -> "2"
    c = Joint 3 -- J2$0 -> c
    d = EndState 4 -- E1$0 ->d
-- T1$0 -> "-1", T2$0 -> "-2", EmptyTrigger$0 -> ""

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