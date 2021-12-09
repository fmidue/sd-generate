// About start states
module startstate_rules // Most constraints of start states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

fact{
	all s1: StartState | s1 not in Node.flowto_triggerwith[Name] // No coming transitions to start states

	// Start states are only left by arrows pointing to something in their own compound state or deeper.
	all s1: StartState, h1: HierarchicalState | s1 in h1.h_contains 
		=> s1.flowto_triggerwith[EmptyName] in getAllNodeInSameAndDeeperLevel[h1] // When start states in hierarchical states
	all s1: StartState, r1:Region | s1 in r1.r_contains 
		=> s1.flowto_triggerwith[EmptyName] in getAllNodeInSameAndDeeperLevel[r1] // When start states in regions	

	all r1: Region | lone (StartState & r1.r_contains)  // In regions, there is at most one start state.
	all h1: HierarchicalState | lone (StartState & h1.h_contains) // In hierarchical states, there is at most one start state.
	one s1: StartState | s1 not in (HierarchicalState.h_contains + Region.r_contains) // Outside all hierarchical states and regions, there is exactly one start state, otherwise the diagram can't be activated
}
