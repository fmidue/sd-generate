// About start states
module startstate_rules // Most constraints of start states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

fact{
	all s1: StartState | s1 not in Node.flowto_triggerwith[Trigger] // No coming transitions to start states

	// Start states are only left by arrows pointing to something in their own compound state or deeper.
	all s1: StartState, c1: CompositeState | s1 in c1.contains => s1.flowto_triggerwith[Trigger] in getAllNodeInSameAndDeeperLevel[c1] // When start states in composite states
	all s1: StartState, r1:Region | s1 in r1.contains => s1.flowto_triggerwith[Trigger] in getAllNodeInSameAndDeeperLevel[r1] // When start states in regions	

	all r1: Region | #(StartState & r1.contains) < 2 // In regions, there is at most one start state.
	all c1: CompositeState | #(StartState & c1.contains) < 2 // In composite states, there is at most one start state.
	one s1: StartState | s1 not in (CompositeState.contains + Region.contains) // Outside all composite states and regions, there is exactly one start state, otherwise the diagram can't be activated
}
