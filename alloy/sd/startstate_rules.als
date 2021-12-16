// About start states
module startstate_rules // Most constraints of start states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

// There is at most one start state in each level
pred atMostOneStartNodesInLevels{
	all r1: Regions | lone (StartNodes & r1.contains)  // In regions, there is at most one start state.
	all h1: HierarchicalStates | lone (StartNodes & h1.contains) // In hierarchical states, there is at most one start state.
	lone s1: StartNodes | s1 not in allContainedNodes // Outside all hierarchical states and regions, there is at most one start state
}

// Start states are only left by arrows pointing to something in their own compound state or deeper.
pred startStatesArrowsPointToSameOrDeeperLevels{
	all s1: StartNodes, h1: HierarchicalStates | 
		s1 in h1.contains 
			implies s1.flow[EmptyTrigger] in nodesInThisAndDeeper[h1] // When start states in hierarchical states
	all s1: StartNodes, r1:Regions | 
		s1 in r1.contains  
			implies s1.flow[EmptyTrigger] in nodesInThisAndDeeper[r1] // When start states in regions	
}

fact{	
	atMostOneStartNodesInLevels
	startStatesArrowsPointToSameOrDeeperLevels
}
