module diagram // name: "", (irrelevant) label: 7
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
one sig N_2 extends HierarchicalStates{}{
  name = Name2 // "B"
  contains = S_2 + N_2_1 + N_2_2
}
one sig S_2 extends StartNodes{}
one sig S_2Flow extends Flows{}{
  from = S_2
  label = EmptyTrigger
  to = N_2_1
}
one sig N_2_1 extends NormalStates{}{
  name = Name3 // "C"
}
one sig N_2_2 extends HierarchicalStates{}{
  name = Name4 // "D"
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





one sig Connection1 extends Flows{}{
  from = N_1
  label = T1 // "a"
  to = N_2_1
}
one sig Connection2 extends Flows{}{
  from = N_2_1
  label = T2 // "b"
  to = N_2_2
}
one sig Connection3 extends Flows{}{
  from = N_2_2_1
  label = T3 // "c"
  to = N_2_2_2
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
  // Flows = SFlow + S_2Flow + S_2_2Flow + Connection1 + Connection2 + Connection3
  // no EndNodes
  // some StartNodes
  // some ComponentNames
  // some TriggerNames
  // some NormalStates
  // some HierarchicalStates
  // no RegionsStates
  // no DeepHistoryNodes
  // no ShallowHistoryNodes
  // no ForkNodes
  // no JoinNodes
}
run {} for 0 EndNodes, 0 RegionsStates, 0 Regions, 0 DeepHistoryNodes, 0 ShallowHistoryNodes, 0 ForkNodes, 0 JoinNodes, 7 ProtoFlows, exactly 6 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
