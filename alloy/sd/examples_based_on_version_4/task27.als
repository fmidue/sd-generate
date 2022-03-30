module diagram // name: "", (irrelevant) label: 12
open uml_state_diagram
one sig S extends StartNodes{}
one sig SFlow extends Flows{}{
  from = S
  label = EmptyTrigger
  to = N_1
}
one sig N_1 extends NormalStates{}{
  name = Name1 // "CD"
}
one sig N_2 extends HierarchicalStates{}{
  name = Name2 // "Radio"
  contains = N_2_1 + N_2_2 + N_2_3 + N_2_0
}
one sig N_2_1 extends NormalStates{}{
  name = Name3 // "1"
}
one sig N_2_2 extends NormalStates{}{
  name = Name4 // "2"
}
one sig N_2_3 extends NormalStates{}{
  name = Name5 // "3"
}
one sig N_2_0 extends ShallowHistoryNodes{}



one sig Connection1 extends Flows{}{
  from = N_1
  label = T1 // "F"
  to = N_2_0
}
one sig Connection2 extends Flows{}{
  from = N_2
  label = T1 // "F"
  to = N_1
}
one sig Connection3 extends Flows{}{
  from = N_2_0
  label = EmptyTrigger
  to = N_2_1
}
one sig Connection4 extends Flows{}{
  from = N_2_1
  label = T2 // "+"
  to = N_2_2
}
one sig Connection5 extends Flows{}{
  from = N_2_2
  label = T2 // "+"
  to = N_2_3
}
one sig Connection6 extends Flows{}{
  from = N_2_3
  label = T3 // "-"
  to = N_2_2
}
one sig Connection7 extends Flows{}{
  from = N_2_2
  label = T3 // "-"
  to = N_2_1
}

one sig Name1 extends ComponentNames{}
one sig Name2 extends ComponentNames{}
one sig Name3 extends ComponentNames{}
one sig Name4 extends ComponentNames{}
one sig Name5 extends ComponentNames{}

one sig T1 extends TriggerNames{}
one sig T2 extends TriggerNames{}
one sig T3 extends TriggerNames{}

run {} for
  0 EndNodes,
  0 RegionsStates,
  0 Regions,
  0 DeepHistoryNodes,
  0 ForkNodes,
  0 JoinNodes,
  12 ProtoFlows, exactly 8 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
