// About reachability
module reachability_rules // most constraints of reachability, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	// all nodes(fork, join, history), start states, end states are constraint under the signature directly
	all n1: NormalState | n1 in (NormalState.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger])
	all c1: CompositeState | (c1.contains + c1.inner.contains) not in (NormalState.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger]) => c1 in (NormalState.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger])
}
