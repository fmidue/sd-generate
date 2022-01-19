// About start states
module startstate_rules // Most constraints of start states, but some constraints are directly with the signature

open components_sig as components // import all signatures

// There is at most one start state at each level
pred atMostOneStartNodesInLevels{
        all r1: Regions | lone (StartNodes & r1.contains)  // In regions, there is at most one start state.
        all h1: HierarchicalStates | lone (StartNodes & h1.contains) // In hierarchical states, there is at most one start state.
        lone (StartNodes - allContainedNodes) // Outside all hierarchical states and regions, there is at most one start state
}

fact{
        atMostOneStartNodesInLevels
}
