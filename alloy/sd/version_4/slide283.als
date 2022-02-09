module diagram // name: "", (irrelevant) label: 12
open uml_state_diagram
one sig S extends StartNodes{}
one sig SFlow extends Flows{}{
  from = S
  label = EmptyTrigger
  to = N_7
}
one sig N_1 extends NormalStates{}{
  name = Name1 // "A(C)"
}
one sig N_2 extends NormalStates{}{
  name = Name2 // "B(C)"
}
one sig N_3 extends NormalStates{}{
  name = Name3 // "A(D)"
}
one sig N_4 extends NormalStates{}{
  name = Name4 // "B(D)"
}
one sig N_5 extends NormalStates{}{
  name = Name5 // "C"
}
one sig N_6 extends NormalStates{}{
  name = Name6 // "D"
}
one sig N_7 extends NormalStates{}{
  name = Name7 // "X()"
}

one sig Connection1 extends Flows{}{
  from = N_1
  label = T1 // "a"
  to = N_2
}
one sig Connection2 extends Flows{}{
  from = N_2
  label = T2 // "f"
  to = N_1
}
one sig Connection3 extends Flows{}{
  from = N_3
  label = T1 // "a"
  to = N_4
}
one sig Connection4 extends Flows{}{
  from = N_4
  label = T2 // "f"
  to = N_3
}
one sig Connection5 extends Flows{}{
  from = N_2
  label = T3 // "b"
  to = N_5
}
one sig Connection6 extends Flows{}{
  from = N_4
  label = T3 // "b"
  to = N_6
}
one sig Connection7 extends Flows{}{
  from = N_5
  label = T4 // "c"
  to = N_6
}
one sig Connection8 extends Flows{}{
  from = N_6
  label = T5 // "d"
  to = N_5
}
one sig Connection9 extends Flows{}{
  from = N_6
  label = T6 // "e"
  to = N_3
}
one sig Connection10 extends Flows{}{
  from = N_5
  label = T6 // "e"
  to = N_1
}
one sig Connection11 extends Flows{}{
  from = N_7
  label = T7 // "x"
  to = N_5
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
one sig T7 extends TriggerNames{}

run {} for
  0 EndNodes,
  0 HierarchicalStates,
  0 RegionsStates,
  0 Regions,
  0 DeepHistoryNodes,
  0 ShallowHistoryNodes,
  0 ForkNodes,
  0 JoinNodes,
  12 ProtoFlows, exactly 12 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
