// About start states
module startstate_rules // most constraints of start states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

fact{
	#StartState > 0 // at least one start state
	all s1: StartState, t1: Trigger | (s1.flow_triggerwith[t1] != none || s1 in Node.flowfrom_triggerwith[t1]) => t1.notated = none // Start states are not left by an arrow with non-empty transition label
	all s1: StartState| s1 not in State.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger] // no transitions to start states
			    && s1 not in JoinNode.flowfrom_triggerwith[Trigger] // no transitions between start states and join nodes

	// Start states are only left by arrows pointing to something in their own compound state or deeper.
	all s1: StartState, c1: CompositeState | s1 in (c1.contains + c1.inner.contains) => s1.flow_triggerwith[Trigger] in (c1.contains + c1.inner.contains) 
	
	all s1: StartState, r1: Region | s1 in r1.contains => #s1 < 2 // In regions, there is at most one start state.
	all s1: StartState, c1: CompositeState | s1 in c1.contains => #s1 < 2 // In composite states, there is at most one start state.
	all s1: StartState | s1 not in (CompositeState.contains + Region.contains) => #s1 < 2 //Outside composite states and regions, there is also at most one start state.
}
