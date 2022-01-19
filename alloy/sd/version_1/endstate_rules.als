// About end states
module endstate_rules // Most constraints of end states, but some constraints are directly with the signature

open components_sig as components // import all signatures

// There is at most one end state at each level
pred atMostOneEndNodesInLevels{
        all r1: Regions | lone (EndNodes & r1.contains) // In regions, there is at most one end state.
        all h1: HierarchicalStates | lone (EndNodes & h1.contains) // In hierarchical states, there is at most one end state.
        lone (EndNodes - allContainedNodes) // Outside hierarchical states and regions, there is also at most one end state.
}

fact{
        atMostOneEndNodesInLevels
}
