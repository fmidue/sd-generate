// About "substates"
module substate_rules // Most constraints of "substates", but some constraints are directly with the signatures

open components_sig as components // import all signatures

fact{
        // No compound or region may be empty or contain only history/fork/join nodes.
        some (States + EndNodes) // It constrains that the whole model can't be empty or contain only only history/fork/join nodes.
        all h1: HierarchicalStates | some (h1.contains & (States + EndNodes)) // It constrains all composite states without regions
        all r1: Regions | some (r1.contains & (States + EndNodes)) // It constrains all region levels
}

// A composite state can't appear in in deeper level of itself
pred acyclicContain{
        all c1: CompositeStates | c1 not in nodesInThisAndDeeper[c1] // A composite state can't appear in deeper level of itself
}

fact{
        acyclicContain
        disj[Regions.contains, HierarchicalStates.contains] // Regions and hierarchical states can't contain same nodes.
}