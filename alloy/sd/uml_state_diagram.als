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


// Other rules
fact{
	// A composite state can't appear in in deeper level of itself
	no h1: HierarchicalState | h1 in getAllNodeInSameAndDeeperLevel [h1]  // A hierarchical state can't appear in deeper level of itself
	no r1: RegionsState | r1 in getAllNodeInSameAndDeeperLevel [r1.inner] // A region state can't appear in deeper level of itself
	
	all r1: Region, h1: HierarchicalState | r1.r_contains & h1.h_contains = none // No same nodes are contained by different objects	
}

run {} for 10
