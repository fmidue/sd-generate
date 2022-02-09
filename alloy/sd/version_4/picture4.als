module diagram // name: "", (irrelevant) label: 14
open uml_state_diagram
one sig S extends StartNodes{}
one sig SFlow extends Flows{}{
  from = S
  label = EmptyTrigger
  to = N_1
}
one sig N_1 extends HierarchicalStates{}{
  name = Name1 // "Composite State"
  contains = S_1 + N_1_1 + N_1_2 + N_1_3
}
one sig S_1 extends StartNodes{}
one sig S_1Flow extends Flows{}{
  from = S_1
  label = EmptyTrigger
  to = N_1_1
}
one sig N_1_1 extends NormalStates{}{
  name = Name2 // "State 1"
}
one sig N_1_2 extends HierarchicalStates{}{
  name = Name3 // "state 2"
  contains = S_1_2 + N_1_2_1 + N_1_2_2
}
one sig S_1_2 extends StartNodes{}
one sig S_1_2Flow extends Flows{}{
  from = S_1_2
  label = EmptyTrigger
  to = N_1_2_1
}
one sig N_1_2_1 extends NormalStates{}{
  name = Name4 // "State 2a"
}
one sig N_1_2_2 extends NormalStates{}{
  name = Name5 // "State 2b"
}


one sig N_1_3 extends DeepHistoryNodes{}


one sig N_2 extends NormalStates{}{
  name = Name6 // "State 3"
}

one sig Connection1 extends Flows{}{
  from = N_1
  label = T1 // "t"
  to = N_2
}
one sig Connection2 extends Flows{}{
  from = N_2
  label = EmptyTrigger
  to = N_1_3
}
one sig Connection3 extends Flows{}{
  from = N_1_1
  label = T2 // "a"
  to = N_1_2
}
one sig Connection4 extends Flows{}{
  from = N_1_2_1
  label = T3 // "b"
  to = N_1_2_2
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
  // Flows = SFlow + S_1Flow + S_1_2Flow + Connection1 + Connection2 + Connection3 + Connection4
  // no EndNodes
  // some StartNodes
  // some ComponentNames
  // some TriggerNames
  // some NormalStates
  // some HierarchicalStates
  // no RegionsStates
  // some DeepHistoryNodes
  // no ShallowHistoryNodes
  // no ForkNodes
  // no JoinNodes
}
run {} for 0 EndNodes, 0 RegionsStates, 0 Regions, 0 ShallowHistoryNodes, 0 ForkNodes, 0 JoinNodes, 14 ProtoFlows, exactly 7 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
