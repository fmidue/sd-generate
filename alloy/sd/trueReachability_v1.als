// About reachability
module trueReachabilit_v1 // A part of reachability

open components_sig as components // import all signatures

// Specialized flattening strategies
pred flatteningRight[fn : Nodes - EndNodes, ds : set ProtoFlows, ts : set (Nodes - StartNodes)]{
        // (Flows & from.fn & to.ts) in ds
        ds.from in fn and ds.to = ts
}

pred flatteningLeft[fs : set (Nodes - EndNodes), ds : set ProtoFlows, tn : Nodes - StartNodes]{
        // (Flows & from.fs & to.tn) in ds
        ds.from = fs and ds.to in tn
}

// It implements true reachability
pred theFlatteningStrategy{
        // The following are predicates to implement "flattening".
        // It flattens flows from composite states to normal states and end nodes
        all pf: ProtoFlows | let sn = StartNodes - allContainedNodes |
                (pf.from in CompositeStates and pf.to in (NormalStates + EndNodes))
                        implies
                                flatteningLeft[States & nodesInThis[pf.from], pf.derived, pf.to]
                        else
                (pf.from in (sn + States) and pf.to in CompositeStates)
                        implies
                                flatteningRight[pf.from, pf.derived, from.(StartNodes & nodesInThis[pf.to]).to]
                        else
                // // It flattens flows from all states and the outermost start node to all states in regions, here end nodes are excluded, because coming to an end node means all end.
                (pf.from in (sn + States) and pf.to in (Regions.contains & States))
                        implies
                                flatteningRight[pf.from, pf.derived, from.(nodesInOtherParallelRegions[(Regions <: contains).(pf.to)] & StartNodes).to]
                        else
                // It seems that above 3 predicates can constrain nodes except special nodes.
                // It flattens flows from all states and the outermost start node to fork nodes
                (pf.from in (sn + States) and pf.to in ForkNodes)
                        implies
                                flatteningRight[pf.from, pf.derived, from.(pf.to).to + from.(nodesInOtherParallelRegions[(Regions <: contains).(from.(pf.to).to)] & StartNodes).to]
                        else
                // It flattens flows from join nodes to all states and the outermost start node
                (pf.from in States & Regions.contains and pf.to in JoinNodes)
                        implies
                                flatteningLeft[pf.from, pf.derived, from.(pf.to).to]
                        else
                // The following 4 predicates flatten flows from states and the outermost start node to history nodes
                // When a history node is in a region and has no default flow (states + the outermost start state -> history nodes in regions and without a default flow)
                (pf.from in (sn + States) and pf.to in (HistoryNodes & Regions.contains) and no from.(pf.to))
                        implies
                                flatteningRight[pf.from, pf.derived, from.(StartNodes & ((RegionsStates <: contains).contains.(pf.to)).contains.contains).to]
                        else
                // When a history node is in a region and has a default flow (states + the outermost start state -> history nodes in regions and with a default flow)
                (pf.from in (sn + States) and pf.to in (HistoryNodes & Regions.contains) and one from.(pf.to))
                        implies
                                flatteningRight[pf.from, pf.derived, (Flows <: from).(pf.to).to + from.(StartNodes & nodesInOtherParallelRegions[contains.(pf.to)]).to]
                        else
                // When a history node is in a hierarchical state and has no default flow (states + the outermost start state -> history nodes in hierarchical states and without a default flow)
                (pf.from in (sn + States) and pf.to in (HistoryNodes & HierarchicalStates.contains) and no from.(pf.to))
                        implies
                                flatteningRight[pf.from, pf.derived, from.(StartNodes & (HierarchicalStates <: contains).(pf.to).contains).to]
                        else
                // When a history node is in a hierarchical state and has a default flow (states + the outermost start state -> history nodes in hierarchical states and with a default flow)
                (pf.from in (sn + States) and pf.to in (HistoryNodes & HierarchicalStates.contains) and one from.(pf.to))
                        implies
                                flatteningRight[pf.from, pf.derived, from.(pf.to).to]
                        else no pf.derived
}
