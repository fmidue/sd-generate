module diagram // name: "order of management system", (irrelevant) label: 7
open uml_state_diagram
one sig S extends StartNodes{}
one sig SFlow extends Flows{}{
  from = S
  label = EmptyTrigger
  to = N_1
}
one sig N_1 extends NormalStates{}{
  name = Name1 // "idle"
}
one sig N_2 extends NormalStates{}{
  name = Name2 // "Send order request"
}
one sig N_3 extends NormalStates{}{
  name = Name3 // "Select normal or special order"
}
one sig N_4 extends NormalStates{}{
  name = Name4 // "Order confirmation"
}
one sig N_5 extends NormalStates{}{
  name = Name5 // "Dispatch order"
}
one sig N_6 extends EndNodes{}

one sig Connection1 extends Flows{}{
  from = N_1
  label = EmptyTrigger
  to = N_2
}
one sig Connection2 extends Flows{}{
  from = N_2
  label = T1 // "Action"
  to = N_3
}
one sig Connection3 extends Flows{}{
  from = N_3
  label = T2 // "Confirm order(Event)"
  to = N_4
}
one sig Connection4 extends Flows{}{
  from = N_4
  label = EmptyTrigger
  to = N_5
}
one sig Connection5 extends Flows{}{
  from = N_2
  label = T3 // "exit"
  to = N_6
}
one sig Connection6 extends Flows{}{
  from = N_5
  label = T4 // "complete"
  to = N_6
}

one sig Name1 extends ComponentNames{}
one sig Name2 extends ComponentNames{}
one sig Name3 extends ComponentNames{}
one sig Name4 extends ComponentNames{}
one sig Name5 extends ComponentNames{}

one sig T1 extends TriggerNames{}
one sig T2 extends TriggerNames{}
one sig T3 extends TriggerNames{}
one sig T4 extends TriggerNames{}

run {} for
  0 HierarchicalStates,
  0 RegionsStates,
  0 Regions,
  0 DeepHistoryNodes,
  0 ShallowHistoryNodes,
  0 ForkNodes,
  0 JoinNodes,
  7 ProtoFlows, exactly 7 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
