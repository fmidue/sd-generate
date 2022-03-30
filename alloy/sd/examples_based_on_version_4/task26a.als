module diagram // name: "", (irrelevant) label: 23
open uml_state_diagram
one sig S extends StartNodes{}
one sig SFlow extends Flows{}{
  from = S
  label = EmptyTrigger
  to = N_1_1_2
}
one sig N_1 extends RegionsStates{}{
  contains = R_1_1 + R_1_2
}
one sig R_1_1 extends Regions{}{
  no name
  contains = S_1_1 + N_1_1_1 + N_1_1_2 + N_1_1_3
}
one sig S_1_1 extends StartNodes{}
one sig S_1_1Flow extends Flows{}{
  from = S_1_1
  label = EmptyTrigger
  to = N_1_1_1
}
one sig N_1_1_1 extends NormalStates{}{
  name = Name1 // "A"
}
one sig N_1_1_2 extends NormalStates{}{
  name = Name2 // "B"
}
one sig N_1_1_3 extends NormalStates{}{
  name = Name3 // "C"
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
  name = Name4 // "D"
}
one sig N_1_2_2 extends NormalStates{}{
  name = Name5 // "E"
}




one sig N_2 extends JoinNodes{}
one sig N_3 extends NormalStates{}{
  name = Name6 // "F"
}
one sig N_4 extends HierarchicalStates{}{
  no name
  contains = S_4 + N_4_1 + N_4_2
}
one sig S_4 extends StartNodes{}
one sig S_4Flow extends Flows{}{
  from = S_4
  label = EmptyTrigger
  to = N_4_2
}
one sig N_4_1 extends NormalStates{}{
  name = Name7 // "G"
}
one sig N_4_2 extends NormalStates{}{
  name = Name8 // "H"
}



one sig Connection1 extends Flows{}{
  from = N_1_1_3
  label = T1 // "e"
  to = N_2
}
one sig Connection2 extends Flows{}{
  from = N_1_2_2
  label = T1 // "e"
  to = N_2
}
one sig Connection3 extends Flows{}{
  from = N_2
  label = EmptyTrigger
  to = N_3
}
one sig Connection4 extends Flows{}{
  from = N_3
  label = T2 // "c"
  to = N_4
}
one sig Connection5 extends Flows{}{
  from = N_3
  label = T3 // "d"
  to = N_4_2
}
one sig Connection6 extends Flows{}{
  from = N_4_2
  label = T4 // "a"
  to = N_1_2_2
}
one sig Connection7 extends Flows{}{
  from = N_4_1
  label = T4 // "a"
  to = N_1
}
one sig Connection8 extends Flows{}{
  from = N_1_1_1
  label = T4 // "a"
  to = N_1_1_2
}
one sig Connection9 extends Flows{}{
  from = N_1_1_2
  label = T5 // "b"
  to = N_1_1_3
}
one sig Connection10 extends Flows{}{
  from = N_1_1_3
  label = T4 // "a"
  to = N_1_1_1
}
one sig Connection11 extends Flows{}{
  from = N_1_2_1
  label = T5 // "b"
  to = N_1_2_2
}
one sig Connection12 extends Flows{}{
  from = N_1_2_2
  label = T2 // "c"
  to = N_1_2_1
}
one sig Connection13 extends Flows{}{
  from = N_4_2
  label = T3 // "d"
  to = N_4_1
}

one sig Name1 extends ComponentNames{}
one sig Name2 extends ComponentNames{}
one sig Name3 extends ComponentNames{}
one sig Name4 extends ComponentNames{}
one sig Name5 extends ComponentNames{}
one sig Name6 extends ComponentNames{}
one sig Name7 extends ComponentNames{}
one sig Name8 extends ComponentNames{}

one sig T1 extends TriggerNames{}
one sig T2 extends TriggerNames{}
one sig T3 extends TriggerNames{}
one sig T4 extends TriggerNames{}
one sig T5 extends TriggerNames{}

run {} for
  0 EndNodes,
  0 DeepHistoryNodes,
  0 ShallowHistoryNodes,
  0 ForkNodes,
  23 ProtoFlows, exactly 17 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
