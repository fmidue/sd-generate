module uml_state_diagram // show uml state diagrams

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
	no c1: CompositeState | c1 in c1.^contains // this relation has no loop
	partof = ~inner  //reverse relation
	
	// if a composite state contains region states, then all states are contained by region states directly and in the composite state indirectly, so the composite state doesn't contain any states directly, history as well
	all s1: State | s1 in Region.contains => s1 not in CompositeState.contains
	all h1: History | h1 in (Region.s_possess + Region.d_possess) => h1 not in (CompositeState.s_possess + CompositeState.d_possess)
	all c1: CompositeState, s1: State | #c1.inner > 0 => s1 not in c1.contains
	all c1: CompositeState, h1: History | #c1.inner > 0 => h1 not in (c1.s_possess + c1.d_possess)
}

//check
run {} for 4 but exactly 2 Region, exactly 1 History, exactly 2 Trigger
