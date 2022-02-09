module diagram // name: "", (irrelevant) label: 35
open uml_state_diagram
one sig S extends StartNodes{}
one sig SFlow extends Flows{}{
  from = S
  label = EmptyTrigger
  to = N_1
}
one sig N_1 extends ForkNodes{}
one sig N_2 extends HierarchicalStates{}{
  name = Name1 // "A"
  contains = N_2_1 + N_2_2
}
one sig N_2_1 extends HierarchicalStates{}{
  name = Name2 // "B"
  contains = N_2_1_1 + N_2_1_2 + N_2_1_3 + N_2_1_0
}
one sig N_2_1_1 extends RegionsStates{}{
  contains = R_2_1_1_1 + R_2_1_1_2
}
one sig R_2_1_1_1 extends Regions{}{
  name = Name3 // "C"
  contains = S_2_1_1_1 + N_2_1_1_1_1 + N_2_1_1_1_2
}
one sig S_2_1_1_1 extends StartNodes{}
one sig S_2_1_1_1Flow extends Flows{}{
  from = S_2_1_1_1
  label = EmptyTrigger
  to = N_2_1_1_1_1
}
one sig N_2_1_1_1_1 extends NormalStates{}{
  name = Name4 // "1"
}
one sig N_2_1_1_1_2 extends NormalStates{}{
  name = Name5 // "2"
}


one sig R_2_1_1_2 extends Regions{}{
  no name
  contains = S_2_1_1_2 + N_2_1_1_2_1 + N_2_1_1_2_2 + N_2_1_1_2_3
}
one sig S_2_1_1_2 extends StartNodes{}
one sig S_2_1_1_2Flow extends Flows{}{
  from = S_2_1_1_2
  label = EmptyTrigger
  to = N_2_1_1_2_1
}
one sig N_2_1_1_2_1 extends NormalStates{}{
  name = Name6 // "3"
}
one sig N_2_1_1_2_2 extends NormalStates{}{
  name = Name7 // "4"
}
one sig N_2_1_1_2_3 extends NormalStates{}{
  name = Name8 // "5"
}




one sig N_2_1_2 extends HierarchicalStates{}{
  name = Name9 // "D"
  contains = S_2_1_2 + N_2_1_2_1 + N_2_1_2_2
}
one sig S_2_1_2 extends StartNodes{}
one sig S_2_1_2Flow extends Flows{}{
  from = S_2_1_2
  label = EmptyTrigger
  to = N_2_1_2_1
}
one sig N_2_1_2_1 extends NormalStates{}{
  name = Name10 // "9"
}
one sig N_2_1_2_2 extends NormalStates{}{
  name = Name11 // "10"
}


one sig N_2_1_3 extends JoinNodes{}
one sig N_2_1_0 extends ShallowHistoryNodes{}


one sig N_2_2 extends HierarchicalStates{}{
  name = Name12 // "E"
  contains = S_2_2 + N_2_2_1 + N_2_2_2 + N_2_2_3
}
one sig S_2_2 extends StartNodes{}
one sig S_2_2Flow extends Flows{}{
  from = S_2_2
  label = EmptyTrigger
  to = N_2_2_3
}
one sig N_2_2_1 extends NormalStates{}{
  name = Name13 // "6"
}
one sig N_2_2_2 extends NormalStates{}{
  name = Name14 // "7"
}
one sig N_2_2_3 extends NormalStates{}{
  name = Name15 // "8"
}





one sig Connection1 extends Flows{}{
  from = N_1
  label = EmptyTrigger
  to = N_2_1_1_1_1
}
one sig Connection2 extends Flows{}{
  from = N_1
  label = EmptyTrigger
  to = N_2_1_1_2_1
}
one sig Connection3 extends Flows{}{
  from = N_2_1_1_1_1
  label = T1 // "i"
  to = N_2_2
}
one sig Connection4 extends Flows{}{
  from = N_2_1_3
  label = EmptyTrigger
  to = N_2_2
}
one sig Connection5 extends Flows{}{
  from = N_2_1_2_2
  label = T2 // "h"
  to = N_2_2_2
}
one sig Connection6 extends Flows{}{
  from = N_2_2_2
  label = T3 // "e"
  to = N_2_1_1_2_3
}
one sig Connection7 extends Flows{}{
  from = N_2_2_1
  label = T4 // "f"
  to = N_2_1_0
}
one sig Connection8 extends Flows{}{
  from = N_2_1_1_1_2
  label = T5 // "c"
  to = N_2_1_3
}
one sig Connection9 extends Flows{}{
  from = N_2_1_1_2_2
  label = T5 // "c"
  to = N_2_1_3
}
one sig Connection10 extends Flows{}{
  from = N_2_1_0
  label = EmptyTrigger
  to = N_2_1_2
}
one sig Connection11 extends Flows{}{
  from = N_2_1_1_1_1
  label = T6 // "a"
  to = N_2_1_1_1_2
}
one sig Connection12 extends Flows{}{
  from = N_2_1_1_1_2
  label = T7 // "b"
  to = N_2_1_1_1_1
}
one sig Connection13 extends Flows{}{
  from = N_2_1_1_2_1
  label = T7 // "b"
  to = N_2_1_1_2_2
}
one sig Connection14 extends Flows{}{
  from = N_2_1_1_2_2
  label = T7 // "b"
  to = N_2_1_1_2_3
}
one sig Connection15 extends Flows{}{
  from = N_2_1_1_2_3
  label = T7 // "b"
  to = N_2_1_1_2_1
}
one sig Connection16 extends Flows{}{
  from = N_2_1_2_1
  label = T8 // "g"
  to = N_2_1_2_2
}
one sig Connection17 extends Flows{}{
  from = N_2_1_2_2
  label = T8 // "g"
  to = N_2_1_2_1
}
one sig Connection18 extends Flows{}{
  from = N_2_2_1
  label = T9 // "d"
  to = N_2_2_3
}
one sig Connection19 extends Flows{}{
  from = N_2_2_3
  label = T9 // "d"
  to = N_2_2_2
}
one sig Connection20 extends Flows{}{
  from = N_2_2_2
  label = T9 // "d"
  to = N_2_2_1
}

one sig Name1 extends ComponentNames{}
one sig Name2 extends ComponentNames{}
one sig Name3 extends ComponentNames{}
one sig Name4 extends ComponentNames{}
one sig Name5 extends ComponentNames{}
one sig Name6 extends ComponentNames{}
one sig Name7 extends ComponentNames{}
one sig Name8 extends ComponentNames{}
one sig Name9 extends ComponentNames{}
one sig Name10 extends ComponentNames{}
one sig Name11 extends ComponentNames{}
one sig Name12 extends ComponentNames{}
one sig Name13 extends ComponentNames{}
one sig Name14 extends ComponentNames{}
one sig Name15 extends ComponentNames{}

one sig T1 extends TriggerNames{}
one sig T2 extends TriggerNames{}
one sig T3 extends TriggerNames{}
one sig T4 extends TriggerNames{}
one sig T5 extends TriggerNames{}
one sig T6 extends TriggerNames{}
one sig T7 extends TriggerNames{}
one sig T8 extends TriggerNames{}
one sig T9 extends TriggerNames{}

fact{
  // Flows = SFlow + S_2_1_1_1Flow + S_2_1_1_2Flow + S_2_1_2Flow + S_2_2Flow + Connection1 + Connection2 + Connection3 + Connection4 + Connection5 + Connection6 + Connection7 + Connection8 + Connection9 + Connection10 + Connection11 + Connection12 + Connection13 + Connection14 + Connection15 + Connection16 + Connection17 + Connection18 + Connection19 + Connection20
  // no EndNodes
  // some StartNodes
  // some ComponentNames
  // some TriggerNames
  // some NormalStates
  // some HierarchicalStates
  // some RegionsStates
  // no DeepHistoryNodes
  // some ShallowHistoryNodes
  // some ForkNodes
  // some JoinNodes
}
run {} for 0 EndNodes, 0 DeepHistoryNodes, 35 ProtoFlows, exactly 25 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
