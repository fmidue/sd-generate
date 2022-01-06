// About reachability
module trueReachabilit_v1 // A part of reachability

open components_sig as components // import all signatures

// Specialized flattening strategies
pred flatteningRight[fn : Nodes - EndNodes, ds : set (ProtoFlows- Flows), ts : set (Nodes - StartNodes)]{
        all tn : ts | one tf : ds | tf.from = fn and tf.to = tn
        all tf : ds | tf.from = fn and tf.to in ts
}

pred flatteningLeft[fs : set (Nodes - EndNodes), ds : set (ProtoFlows- Flows), tn : Nodes - StartNodes]{
        all fn : fs | one tf : ds | tf.from = fn and tf.to = tn
        all tf : ds | tf.from in fs and tf.to = tn
}

// It implements true reachability
pred theFlatteningStrategy{
        // The following are predicates to implement "flattening".
        // It flattens flows from composite states to normal states and end nodes
        all pf: ProtoFlows |
                (pf.from in CompositeStates and pf.to in (NormalStates + EndNodes))
                        implies (let cs = States & nodesInThis[pf.from] |
                                flatteningLeft[cs, pf.derived, pf.to])
                        else
                (pf.from in States and pf.to in CompositeStates)
                        implies (let cs = from.(StartNodes & nodesInThis[pf.to]).to |
                                flatteningRight[pf.from, pf.derived, cs])
                        else
                // // It flattens flows from all states and the outermost start node to all states in regions, here end nodes are excluded, because coming to an end node means all end.
                (pf.from in States and pf.to in (Regions.contains & States))
                        implies (let cs = from.(nodesInOtherParallelRegions[(Regions <: contains).(pf.to)] & StartNodes).to |
                                flatteningRight[pf.from, pf.derived, cs])
                        else
                // It seems that above 3 predicates can constrain nodes except special nodes.
                // It flattens flows from all states and the outermost start node to fork nodes
                (pf.from in States and pf.to in ForkNodes)
                        implies (let cs = from.(pf.to).to + from.(nodesInOtherParallelRegions[(Regions <: contains).(from.(pf.to).to)] & StartNodes).to |
                                flatteningRight[pf.from, pf.derived, cs])
                        else
                // It flattens flows from join nodes to all states and the outermost start node
                (pf.from in States & Regions.contains and pf.to in JoinNodes)
                        implies (let cs = pf.from | // Here, I omit flattening all states in other parallel regions, because it doesn't have impact on judging reachability, but if it is added, it will increase considerable complexity and recursions will exist
                                flatteningLeft[cs, pf.derived, from.(pf.to).to])
                        else
                // The following 4 predicates flatten flows from states and the outermost start node to history nodes
                // When a history node is in a region and has no default flow (states + the outermost start state -> history nodes in regions and without a default flow)
                (pf.from in States and pf.to in (HistoryNodes & Regions.contains) and no from.(pf.to))
                        implies (let cs = from.(StartNodes & ((RegionsStates <: contains).contains.(pf.to)).contains.contains).to |
                                flatteningRight[pf.from, pf.derived, cs])
                        else
                // When a history node is in a region and has a default flow (states + the outermost start state -> history nodes in regions and with a default flow)
                (pf.from in States and pf.to in (HistoryNodes & Regions.contains) and one from.(pf.to))
                        implies (let cs = (Flows <: from).(pf.to).to + from.(nodesInOtherParallelRegions[contains.(pf.to)]).to |
                                flatteningRight[pf.from, pf.derived, cs])
                        else
                // When a history node is in a hierarchical state and has no default flow (states + the outermost start state -> history nodes in hierarchical states and without a default flow)
                (pf.from in States and pf.to in (HistoryNodes & HierarchicalStates.contains) and no from.(pf.to))
                        implies (let cs = from.(StartNodes & (HierarchicalStates <: contains).(pf.to).contains).to |
                                flatteningRight[pf.from, pf.derived, cs])
                        else
                // When a history node is in a hierarchical state and has a default flow (states + the outermost start state -> history nodes in hierarchical states and with a default flow)
                (pf.from in States and pf.to in (HistoryNodes & HierarchicalStates.contains) and one from.(pf.to))
                        implies (let cs = from.(pf.to).to |
                                flatteningRight[pf.from, pf.derived, cs])
                        else no pf.derived
}
