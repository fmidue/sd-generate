module diagram // name: "", (irrelevant) label: 12
open uml_state_diagram
one sig S extends StartNodes{}
one sig SFlow extends Flows{}{
  from = S
  label = EmptyTrigger
  to = N_3
}
one sig N_1 extends RegionsStates{}{
  contains = R_1_1 + R_1_2
}
one sig R_1_1 extends Regions{}{
  no name
  contains = N_1_1_1 + N_1_1_2 + N_1_1_3
}
one sig N_1_1_1 extends NormalStates{}{
  name = Name1 // "Tasse nehmen"
}
one sig N_1_1_2 extends NormalStates{}{
  name = Name2 // "Kaffee trinken"
}
one sig N_1_1_3 extends NormalStates{}{
  name = Name3 // "Tasse absetzen"
}


one sig R_1_2 extends Regions{}{
  no name
  contains = N_1_2_1
}
one sig N_1_2_1 extends NormalStates{}{
  name = Name4 // "Zeitung lesen"
}




one sig N_3 extends ForkNodes{}
one sig N_4 extends JoinNodes{}
one sig N_5 extends EndNodes{}

one sig Connection1 extends Flows{}{
  from = N_3
  label = EmptyTrigger
  to = N_1_1_1
}
one sig Connection2 extends Flows{}{
  from = N_1_1_1
  label = EmptyTrigger
  to = N_1_1_2
}
one sig Connection3 extends Flows{}{
  from = N_1_1_2
  label = EmptyTrigger
  to = N_1_1_3
}
one sig Connection4 extends Flows{}{
  from = N_1_1_3
  label = EmptyTrigger
  to = N_4
}
one sig Connection5 extends Flows{}{
  from = N_3
  label = EmptyTrigger
  to = N_1_2_1
}
one sig Connection6 extends Flows{}{
  from = N_1_2_1
  label = EmptyTrigger
  to = N_4
}
one sig Connection7 extends Flows{}{
  from = N_4
  label = T1 // "b"
  to = N_5
}

one sig Name1 extends ComponentNames{}
one sig Name2 extends ComponentNames{}
one sig Name3 extends ComponentNames{}
one sig Name4 extends ComponentNames{}

one sig T1 extends TriggerNames{}

run {} for
  0 HierarchicalStates,
  0 DeepHistoryNodes,
  0 ShallowHistoryNodes,
  12 ProtoFlows, exactly 8 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
