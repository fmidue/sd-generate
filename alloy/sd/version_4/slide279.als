module diagram // name: "", (irrelevant) label: 16
open uml_state_diagram
one sig S extends StartNodes{}
one sig SFlow extends Flows{}{
  from = S
  label = EmptyTrigger
  to = N_1
}
one sig N_1 extends NormalStates{}{
  name = Name1 // "A"
}
one sig N_2 extends RegionsStates{}{
  contains = R_2_1 + R_2_2
}
one sig R_2_1 extends Regions{}{
  no name
  contains = S_2_1 + N_2_1_1 + N_2_1_2 + N_2_1_3
}
one sig S_2_1 extends StartNodes{}
one sig S_2_1Flow extends Flows{}{
  from = S_2_1
  label = EmptyTrigger
  to = N_2_1_1
}
one sig N_2_1_1 extends NormalStates{}{
  name = Name2 // "B"
}
one sig N_2_1_2 extends NormalStates{}{
  name = Name3 // "C"
}
one sig N_2_1_3 extends NormalStates{}{
  name = Name4 // "D"
}


one sig R_2_2 extends Regions{}{
  no name
  contains = S_2_2 + N_2_2_1 + N_2_2_2
}
one sig S_2_2 extends StartNodes{}
one sig S_2_2Flow extends Flows{}{
  from = S_2_2
  label = EmptyTrigger
  to = N_2_2_1
}
one sig N_2_2_1 extends NormalStates{}{
  name = Name5 // "E"
}
one sig N_2_2_2 extends NormalStates{}{
  name = Name6 // "F"
}




one sig N_3 extends JoinNodes{}
one sig N_4 extends NormalStates{}{
  name = Name7 // "G"
}
one sig N_5 extends EndNodes{}

one sig Connection1 extends Flows{}{
  from = N_1
  label = T1 // "a"
  to = N_2
}
one sig Connection2 extends Flows{}{
  from = N_2_1_2
  label = T2 // "h"
  to = N_4
}
one sig Connection3 extends Flows{}{
  from = N_2_1_3
  label = EmptyTrigger
  to = N_3
}
one sig Connection4 extends Flows{}{
  from = N_2_2_2
  label = EmptyTrigger
  to = N_3
}
one sig Connection5 extends Flows{}{
  from = N_3
  label = T3 // "g"
  to = N_4
}
one sig Connection6 extends Flows{}{
  from = N_4
  label = EmptyTrigger
  to = N_5
}
one sig Connection7 extends Flows{}{
  from = N_2_1_1
  label = T4 // "b"
  to = N_2_1_2
}
one sig Connection8 extends Flows{}{
  from = N_2_1_2
  label = T5 // "c"
  to = N_2_1_3
}
one sig Connection9 extends Flows{}{
  from = N_2_2_1
  label = T6 // "e"
  to = N_2_2_2
}

one sig Name1 extends ComponentNames{}
one sig Name2 extends ComponentNames{}
one sig Name3 extends ComponentNames{}
one sig Name4 extends ComponentNames{}
one sig Name5 extends ComponentNames{}
one sig Name6 extends ComponentNames{}
one sig Name7 extends ComponentNames{}

one sig T1 extends TriggerNames{}
one sig T2 extends TriggerNames{}
one sig T3 extends TriggerNames{}
one sig T4 extends TriggerNames{}
one sig T5 extends TriggerNames{}
one sig T6 extends TriggerNames{}

fact{
  // Flows = SFlow + S_2_1Flow + S_2_2Flow + Connection1 + Connection2 + Connection3 + Connection4 + Connection5 + Connection6 + Connection7 + Connection8 + Connection9
  // some EndNodes
  // some StartNodes
  // some ComponentNames
  // some TriggerNames
  // some NormalStates
  // no HierarchicalStates
  // some RegionsStates
  // no DeepHistoryNodes
  // no ShallowHistoryNodes
  // no ForkNodes
  // some JoinNodes
}
run {} for 0 HierarchicalStates, 0 DeepHistoryNodes, 0 ShallowHistoryNodes, 0 ForkNodes, 16 ProtoFlows, exactly 12 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
