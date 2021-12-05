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
	all c1: CompositeState, n1: Node | #c1.inner > 0 => n1 not in c1.contains
}

// check {all s1, s2: State | s1 != s2 && s1.named != none && s2.named != none => s1.named != s2.named} for 6
run {} for 5 but exactly 1 JoinNode, exactly 1 ForkNode, exactly 2 Trigger
