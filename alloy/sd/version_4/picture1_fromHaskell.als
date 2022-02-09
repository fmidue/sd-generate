module diagram // name: "active", (irrelevant) label: 12
open uml_state_diagram
one sig S extends StartNodes{}
one sig SFlow extends Flows{}{
  from = S
  label = EmptyTrigger
  to = N_1_2_2
}
one sig N_1 extends RegionsStates{}{
  contains = R_1_1 + R_1_2 + R_1_3
}
one sig R_1_1 extends Regions{}{
  no name
  contains = S_1_1 + N_1_1_1 + N_1_1_2
}
one sig S_1_1 extends StartNodes{}
one sig S_1_1Flow extends Flows{}{
  from = S_1_1
  label = EmptyTrigger
  to = N_1_1_1
}
one sig N_1_1_1 extends NormalStates{}{
  name = Name1 // "NumLockOff"
}
one sig N_1_1_2 extends NormalStates{}{
  name = Name2 // "NumLockOn"
}


one sig R_1_2 extends Regions{}{
  no name
  contains = S_1_2 + N_1_2_1 + N_1_2_2
}
one sig S_1_2 extends StartNodes{}
one sig S_1_2Flow extends Flows{}{
  from = S_1_2
  label = EmptyTrigger
  to = N_1_2_1
}
one sig N_1_2_1 extends NormalStates{}{
  name = Name3 // "CapsLockOff"
}
one sig N_1_2_2 extends NormalStates{}{
  name = Name4 // "CapsLockOn"
}


one sig R_1_3 extends Regions{}{
  no name
  contains = S_1_3 + N_1_3_1 + N_1_3_2
}
one sig S_1_3 extends StartNodes{}
one sig S_1_3Flow extends Flows{}{
  from = S_1_3
  label = EmptyTrigger
  to = N_1_3_1
}
one sig N_1_3_1 extends NormalStates{}{
  name = Name5 // "ScrollLockOff"
}
one sig N_1_3_2 extends NormalStates{}{
  name = Name6 // "ScrollLockOn"
}





one sig Connection1 extends Flows{}{
  from = N_1_1_1
  label = T1 // "EvNumLockPressed"
  to = N_1_1_2
}
one sig Connection2 extends Flows{}{
  from = N_1_1_2
  label = T1 // "EvNumLockPressed"
  to = N_1_1_1
}
one sig Connection3 extends Flows{}{
  from = N_1_2_1
  label = T2 // "EvCapsLockPressed"
  to = N_1_2_2
}
one sig Connection4 extends Flows{}{
  from = N_1_2_2
  label = T2 // "EvCapsLockPressed"
  to = N_1_2_1
}
one sig Connection5 extends Flows{}{
  from = N_1_3_1
  label = T3 // "EvScrollLockPressed"
  to = N_1_3_2
}
one sig Connection6 extends Flows{}{
  from = N_1_3_2
  label = T3 // "EvScrollLockPressed"
  to = N_1_3_1
}

one sig Name1 extends ComponentNames{}
one sig Name2 extends ComponentNames{}
one sig Name3 extends ComponentNames{}
one sig Name4 extends ComponentNames{}
one sig Name5 extends ComponentNames{}
one sig Name6 extends ComponentNames{}

one sig T1 extends TriggerNames{}
one sig T2 extends TriggerNames{}
one sig T3 extends TriggerNames{}

fact{
  // Flows = SFlow + S_1_1Flow + S_1_2Flow + S_1_3Flow + Connection1 + Connection2 + Connection3 + Connection4 + Connection5 + Connection6
  // no EndNodes
  // some StartNodes
  // some ComponentNames
  // some TriggerNames
  // some NormalStates
  // no HierarchicalStates
  // some RegionsStates
  // no DeepHistoryNodes
  // no ShallowHistoryNodes
  // no ForkNodes
  // no JoinNodes
}
run {} for 0 EndNodes, 0 HierarchicalStates, 0 DeepHistoryNodes, 0 ShallowHistoryNodes, 0 ForkNodes, 0 JoinNodes, 12 ProtoFlows, exactly 10 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
