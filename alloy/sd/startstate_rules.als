// About start states
module startstate_rules // Most constraints of start states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

fact{
	// Start states are only left by arrows pointing to something in their own compound state or deeper.
	all s1: StartStates, h1: HierarchicalStates | s1 in h1.h_contains 
		=> s1.flowto_triggerwith[EmptyTriggers] in getAllNodesInSameAndDeeperLevel[h1] // When start states in hierarchical states
	all s1: StartStates, r1:Regions | s1 in r1.r_contains 
		=> s1.flowto_triggerwith[EmptyTriggers] in getAllNodesInSameAndDeeperLevel[r1] // When start states in regions	

	all r1: Regions | lone (StartStates & r1.r_contains)  // In regions, there is at most one start state.
	all h1: HierarchicalStates | lone (StartStates & h1.h_contains) // In hierarchical states, there is at most one start state.
	lone s1: StartStates | s1 not in (HierarchicalStates.h_contains + Regions.r_contains) // Outside all hierarchical states and regions, there is at most one start state
}
