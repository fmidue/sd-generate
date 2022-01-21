module uml_state_diagram_version1 // All nodes and naming rules are introduced, but no transitions and further strategies.
/*
These constraints are added:
1. No compound or region may be empty or contain only start/history/fork/join nodes.
2. There is at most one start/end/shallow/deep history in each level.
3. Naming rules(see #6 About names of states (and of regions etc.))
*/
open components_sig as components
open startstate_rules // import constraints of start states
open endstate_rules // import constraints of end states
open substate_rules // import constraints of "substates"
open name_rules // import constraints of names

// There should be at most one shallow/deep history node at each level (optional).
pred atMostOneDeepAndShallowHistoryNodes{
        all r1: Regions | lone (ShallowHistoryNodes & r1.contains) // In regions, there is at most one shallow history.
        all h1: HierarchicalStates | lone (ShallowHistoryNodes & h1.contains) // In composite states, there is at most one shallow history.
        all r1: Regions | lone (DeepHistoryNodes & r1.contains) // In regions, there is at most one deep history.
        all h1: HierarchicalStates | lone (DeepHistoryNodes & h1.contains) // In composite states, there is at most one deep history.
}

// A composite state can't appear in in deeper level of itself
pred acyclicContain{
        all c1: CompositeStates | c1 not in nodesInThisAndDeeper[c1]
}

fact{
        acyclicContain
        disj[Regions.contains, HierarchicalStates.contains] // Regions and hierarchical states can't contain same nodes.
}

run {} for 3 but exactly 1 RegionsStates, exactly 1 HierarchicalStates, exactly 2 Regions, exactly 2 NormalStates, exactly 4 ComponentNames
