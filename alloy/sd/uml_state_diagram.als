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
	no c1: CompositeState | c1 in c1.^contains // This relation has no loop
	partof = ~inner  // Reverse relation
	all r1: Region, c1: CompositeState | r1.contains & c1.contains = none // No same nodes are contained by different objects	

	// If a composite state contains region states, then all states are contained by region states directly and in the composite state indirectly, so the composite state doesn't contain any states directly, history as well
	all c1: CompositeState, n1: Node | #c1.inner > 0 => n1 not in c1.contains
}

run {} for 10
