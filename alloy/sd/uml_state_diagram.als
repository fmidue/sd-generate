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
	no c1: CompositeStateWithoutRegion | c1 in c1.^contains // This relation has no loop
	all r1: Region, c1: CompositeStateWithoutRegion | r1.contains & c1.contains = none // No same nodes are contained by different objects	
}

run {} for 10
