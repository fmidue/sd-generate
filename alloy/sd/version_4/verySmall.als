module diagram // name: "", (irrelevant) label: 1
open uml_state_diagram
one sig S extends StartNodes{}
one sig SFlow extends Flows{}{
  from = S
  label = EmptyTrigger
  to = N_1
}
one sig N_1 extends EndNodes{}




fact{
  // Flows = SFlow
  // some EndNodes
  // some StartNodes
  // no ComponentNames
  // no TriggerNames
  // no NormalStates
  // no HierarchicalStates
  // no RegionsStates
  // no DeepHistoryNodes
  // no ShallowHistoryNodes
  // no ForkNodes
  // no JoinNodes
}
run {} for 0 ComponentNames, 0 TriggerNames, 0 NormalStates, 0 HierarchicalStates, 0 RegionsStates, 0 Regions, 0 DeepHistoryNodes, 0 ShallowHistoryNodes, 0 ForkNodes, 0 JoinNodes, 1 ProtoFlows, exactly 1 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
