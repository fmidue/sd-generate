// About reachability
module reachability_rules // Most constraints of reachability, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	// All nodes(fork, join, history, but no start states) and states are reachable from a start state or somewhere else 
	all n1: (Node - StartState - CompositeState) | n1 in (Node - n1).flowto_triggerwith[Trigger]
	all c1: CompositeState | (c1.contains + c1.inner.contains) & Node.flowto_triggerwith[Trigger] = none => c1 in (Node-c1).flowto_triggerwith[Trigger]
}
