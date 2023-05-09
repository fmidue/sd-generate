// About reachability
module reachability_rules // Most constraints of reachability, but some constraints are directly with the signatures

open components_sig as components // import all signatures
open trueReachability // import "trueReachability"

// Each composite state has at least one entry, except something like "box", in which all events happen, but it also has a default standard entry from the outermost start node which can be set invisible
pred atLeastOneEntryToCompositeStates{
        all c1: CompositeStates |
                let n1 = nodesInThisAndDeeper[c1], n2 = Nodes - c1 - n1 |
                        c1 in (Flows <: from).n2.to // Standard entry
                        or some (n1 & (Flows <: from).n2.to) // Direct, history or fork entry
}

// It implements only approximate reachability
pred approximateReachability{
        no derived
        // Each normal/fork/join/history/end state has at least one incoming arrow (from a start state or somewhere else)
        all n1: (Nodes - StartNodes - CompositeStates) | n1 in (Flows <: from).(Nodes - n1).to
        one (StartNodes - allContainedNodes) // Outside all hierarchical states and regions, there is exactly one start state
        atLeastOneEntryToCompositeStates
}

pred trueReachability{
        atLeastOneEntryToCompositeStates // It is a necessary condition for "true reachability"

        theFlatteningStrategy

        (Nodes - StartNodes - CompositeStates) in
                (StartNodes - allContainedNodes).^(~from.to) // Starting from a outermost start node except start nodes and composite states, all nodes except start nodes and composite states can be reachable
}

fact{
        trueReachability
}
