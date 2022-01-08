// About reachability
module trueReachabilit // A part of reachability

open components_sig as components // import all signatures

// It implements true reachability
pred theFlatteningStrategy{
        // The following are predicates to implement "flattening".
        // It flattens flows from composite states to normal states and end nodes
        all pf: ProtoFlows | let sn = StartNodes - allContainedNodes |
                (pf.from in CompositeStates and pf.to in (NormalStates + EndNodes))
                        implies
                                pf.derived.from = States & nodesInThis[pf.from]
                                and pf.derived.to in pf.to
                        else
                (pf.from in (sn + States) and pf.to in CompositeStates)
                        implies
                                pf.derived.from = pf.from
                                and pf.derived.to = from.(StartNodes & (pf.to).contains).to
                        else
                // // It flattens flows from all states and the outermost start node to all states in regions, here end nodes are excluded, because coming to an end node means all end.
                (pf.from in (sn + States - nodesInThisAndDeeper[(Regions <: contains).(pf.to)]) and pf.to in (Regions.contains & States) and pf not in to.(HistoryNodes + ForkNodes).derived)
                        implies
                                pf.derived.from = pf.from
                                and pf.derived.to = from.(nodesInOtherParallelRegions[(Regions <: contains).(pf.to)] & StartNodes).to
                        else
                // It seems that above 3 predicates can constrain nodes except special nodes.
                // It flattens flows from all states and the outermost start node to fork nodes
                (pf.from in (sn + States) and pf.to in ForkNodes)
                        implies
                                pf.derived.from = pf.from
                                and pf.derived.to = from.(pf.to).to + from.(nodesInOtherParallelRegions[(Regions <: contains).(from.(pf.to).to)] & StartNodes).to
                        else
                // It flattens flows from states in regions to join nodes
                (pf.from in States & Regions.contains and pf.to in JoinNodes)
                        implies
                                pf.derived.from = pf.from
                                and pf.derived.to = from.(pf.to).to
                        else
                // The following 4 predicates flatten flows from states and the outermost start node to history nodes
                // When a history node is in a region and has no default flow (states + the outermost start state -> history nodes in regions and without a default flow)
                (pf.from in (sn + States) and pf.to in (HistoryNodes & Regions.contains) and no from.(pf.to))
                        implies
                                pf.derived.from = pf.from
                                and pf.derived.to = from.(StartNodes & ((RegionsStates <: contains).contains.(pf.to)).contains.contains).to
                        else
                // When a history node is in a region and has a default flow (states + the outermost start state -> history nodes in regions and with a default flow)
                (pf.from in (sn + States) and pf.to in (HistoryNodes & Regions.contains) and one from.(pf.to))
                        implies
                                pf.derived.from = pf.from
                                and pf.derived.to = from.(pf.to).to + from.(StartNodes & nodesInOtherParallelRegions[contains.(pf.to)]).to
                        else
                // When a history node is in a hierarchical state and has no default flow (states + the outermost start state -> history nodes in hierarchical states and without a default flow)
                (pf.from in (sn + States) and pf.to in (HistoryNodes & HierarchicalStates.contains) and no from.(pf.to))
                        implies
                                pf.derived.from = pf.from
                                and pf.derived.to = from.(StartNodes & (HierarchicalStates <: contains).(pf.to).contains).to
                        else
                // When a history node is in a hierarchical state and has a default flow (states + the outermost start state -> history nodes in hierarchical states and with a default flow)
                (pf.from in (sn + States) and pf.to in (HistoryNodes & HierarchicalStates.contains) and one from.(pf.to))
                        implies
                                pf.derived.from = pf.from
                                and pf.derived.to = from.(pf.to).to
                        else no pf.derived
}
