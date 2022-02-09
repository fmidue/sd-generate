module diagram // name: "", (irrelevant) label: 8
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
one sig N_2 extends NormalStates{}{
  name = Name2 // "B"
}
one sig N_3 extends NormalStates{}{
  name = Name3 // "C"
}
one sig N_4 extends NormalStates{}{
  name = Name4 // "D"
}

one sig Connection1 extends Flows{}{
  from = N_1
  label = T1 // "a"
  to = N_3
}
one sig Connection2 extends Flows{}{
  from = N_1
  label = T2 // "f"
  to = N_2
}
one sig Connection3 extends Flows{}{
  from = N_2
  label = T3 // "b"
  to = N_4
}
one sig Connection4 extends Flows{}{
  from = N_3
  label = T4 // "c"
  to = N_4
}
one sig Connection5 extends Flows{}{
  from = N_3
  label = T5 // "e"
  to = N_1
}
one sig Connection6 extends Flows{}{
  from = N_4
  label = T6 // "d"
  to = N_3
}
one sig Connection7 extends Flows{}{
  from = N_4
  label = T5 // "e"
  to = N_1
}

one sig Name1 extends ComponentNames{}
one sig Name2 extends ComponentNames{}
one sig Name3 extends ComponentNames{}
one sig Name4 extends ComponentNames{}

one sig T1 extends TriggerNames{}
one sig T2 extends TriggerNames{}
one sig T3 extends TriggerNames{}
one sig T4 extends TriggerNames{}
one sig T5 extends TriggerNames{}
one sig T6 extends TriggerNames{}

fact{
  // Flows = SFlow + Connection1 + Connection2 + Connection3 + Connection4 + Connection5 + Connection6 + Connection7
  // no EndNodes
  // some StartNodes
  // some ComponentNames
  // some TriggerNames
  // some NormalStates
  // no HierarchicalStates
  // no RegionsStates
  // no DeepHistoryNodes
  // no ShallowHistoryNodes
  // no ForkNodes
  // no JoinNodes
}
run {} for 0 EndNodes, 0 HierarchicalStates, 0 RegionsStates, 0 Regions, 0 DeepHistoryNodes, 0 ShallowHistoryNodes, 0 ForkNodes, 0 JoinNodes, 8 ProtoFlows, exactly 8 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
