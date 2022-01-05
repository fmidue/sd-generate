// About reachability
module trueReachabilit_v2 // A part of reachability

open components_sig as components // import all signatures

// Each composite state has at least one entry, except something like "box", in which all events happen, but it also has a default standard entry from the outermost start node which can be set invisible
pred atLeastOneEntryToCompositeStates{
        all c1: CompositeStates |
                let n1 = nodesInThisAndDeeper[c1], n2 = Nodes - c1 - n1 |
                        c1 in (Flows <: from).n2.to // Standard entry
                        or some (n1 & (Flows <: from).n2.to) // Direct, history or fork entry
}

// Generalized flattening strategy
pred flattening[fs : set (Nodes - EndNodes), ds : set (ProtoFlows - Flows), ts : set (Nodes - StartNodes)]{
        all fn : fs, tn : ts | one tf : ds | tf.from = fn and tf.to = tn
        all tf : ds | tf.from in fs and tf.to in ts
}

// It implements true reachability
pred trueReachability{
        atLeastOneEntryToCompositeStates // It is a necessary condition for "true reachability"
        // The following are predicates to implement "flattening".
        // It flattens flows from composite states to normal states and end nodes
        all pf: ProtoFlows |
                (pf.from in CompositeStates and pf.to in (NormalStates + EndNodes))
                        implies (let cs = States & nodesInThis[pf.from] |
                                flattening[cs, pf.derived, pf.to])
                        else
                (pf.from in States and pf.to in CompositeStates)
                        implies (let cs = from.(StartNodes & nodesInThis[pf.to]).to |
                                flattening[pf.from, pf.derived, cs])
                        else
                // // It flattens flows from all states and the outermost start node to all states in regions, here end nodes are excluded, because coming to an end node means all end.
                (pf.from in States and pf.to in (Regions.contains & States))
                        implies (let cs = from.(nodesInOtherParallelRegions[(Regions <: contains).(pf.to)] & StartNodes).to |
                                flattening[pf.from, pf.derived, cs])
                        else
                // It seems that above 3 predicates can constrain nodes except special nodes.
                // It flattens flows from all states and the outermost start node to fork nodes
                (pf.from in States and pf.to in ForkNodes)
                        implies (let cs = from.(pf.to).to + from.(nodesInOtherParallelRegions[(Regions <: contains).(from.(pf.to).to)] & StartNodes).to |
                                flattening[pf.from, pf.derived, cs])
                        else
                // It flattens flows from join nodes to all states and the outermost start node
                (pf.from in States & Regions.contains and pf.to in JoinNodes)
                        implies (let cs = pf.from | // Here, I omit flattening all states in other parallel regions, because it doesn't have impact on judging reachability, but if it is added, it will increase considerable complexity and recursions will exist
                                flattening[cs, pf.derived, from.(pf.to).to])
                        else
                // The following 4 predicates flatten flows from states and the outermost start node to history nodes
                // When a history node is in a region and has no default flow (states + the outermost start state -> history nodes in regions and without a default flow)
                (pf.from in States and pf.to in (HistoryNodes & Regions.contains) and no from.(pf.to))
                        implies (let cs = from.(StartNodes & ((RegionsStates <: contains).contains.(pf.to)).contains.contains).to |
                                 flattening[pf.from, pf.derived, cs])
                        else
                // When a history node is in a region and has a default flow (states + the outermost start state -> history nodes in regions and with a default flow)
                (pf.from in States and pf.to in (HistoryNodes & Regions.contains) and one from.(pf.to))
                        implies (let cs = (Flows <: from).(pf.to).to + from.(nodesInOtherParallelRegions[contains.(pf.to)]).to |
                                 flattening[pf.from, pf.derived, cs])
                        else
                // When a history node is in a hierarchical state and has no default flow (states + the outermost start state -> history nodes in hierarchical states and without a default flow)
                (pf.from in States and pf.to in (HistoryNodes & HierarchicalStates.contains) and no from.(pf.to))
                        implies (let cs = from.(StartNodes & (HierarchicalStates <: contains).(pf.to).contains).to |
                                 flattening[pf.from, pf.derived, cs])
                        else
                // When a history node is in a hierarchical state and has a default flow (states + the outermost start state -> history nodes in hierarchical states and with a default flow)
                (pf.from in States and pf.to in (HistoryNodes & HierarchicalStates.contains) and one from.(pf.to))
                        implies (let cs = from.(pf.to).to |
                                 flattening[pf.from, pf.derived, cs])
                        else no pf.derived

        (Nodes - StartNodes - CompositeStates) in
                (Nodes - StartNodes - CompositeStates).^(~from.to + ~to.from) // Starting from a random nodes except start nodes and composite states, all nodes except start nodes and composite states can be reachable
}