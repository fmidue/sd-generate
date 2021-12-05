// About start states
module startstate_rules // most constraints of start states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

fact{
	// Start states are only left by arrows pointing to something in their own compound state or deeper.
	all s1: StartState, c1: CompositeState | s1 in c1.contains => s1.flowto_triggerwith[Trigger] in getAllNodeInSameAndDeeperLevel[c1] // When start states in composite states
	all s1: StartState, r1:Region | s1 in r1.contains => s1.flowto_triggerwith[Trigger] in getAllNodeInSameAndDeeperLevel[r1] // When start states in regions
	
	all s1: StartState, r1: Region | s1 in r1.contains=> lone s1 // In regions, there is at most one start state.
	all s1: StartState, c1: CompositeState | s1 in c1.contains=> lone s1 // In composite states, there is at most one start state.
	all s1: StartState | s1 not in (CompositeState.contains + Region.contains) => lone s1 //Outside composite states and regions, there is also at most one start state.
}
