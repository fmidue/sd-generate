module uml_state_diagram // Show uml state diagrams

open components_sig as components // import all signatures
open startstate_rules // import constraints of start states
open endstate_rules // import constraints of end states
open region_rules // import constraints of regions and region states
open node_rules // import constraints of fork and join nodes
open reachability_rules // import constraints of reachability
open history_rules // import constraints of history nodes
open transition_rules // import constraints of transition labels
open substate_rules // import constraints of "substates"
open name_rules // import constraints of names

// A composite state can't appear in in deeper level of itself
pred acyclicContain{
	no h1: HierarchicalStates | h1 in getAllNodesInSameAndDeeperLevels[h1]  // A hierarchical state can't appear in deeper level of itself
	no r1: RegionsStates | r1 in getAllNodesInSameAndDeeperLevels[r1.inner] // A region state can't appear in deeper level of itself
}

// Other rules
fact{
	acyclicContain	
}

run {} for 7 but exactly 2 HierarchicalStates, exactly 4 NormalStates, exactly 2 TriggerNames
