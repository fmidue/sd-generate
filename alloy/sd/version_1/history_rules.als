// About history nodes
module history_rules // Most constraints of history nodes, but some constraints are directly with the signatures

open components_sig as components // import all signatures

// This predication is optional
pred atMostOneDeepAndShallowHistoryNodes{
        all r1: Regions | lone (ShallowHistoryNodes & r1.contains) // In regions, there is at most one shallow history.
        all h1: HierarchicalStates | lone (ShallowHistoryNodes & h1.contains) // In composite states, there is at most one shallow history.
        all r1: Regions | lone (DeepHistoryNodes & r1.contains) // In regions, there is at most one deep history.
        all h1: HierarchicalStates | lone (DeepHistoryNodes & h1.contains) // In composite states, there is at most one deep history.
}

fact{
        // atMostOneDeepAndShallowHistoryNodes // In composite states and regions, there is at most one shallow history and at most one deep history
}
