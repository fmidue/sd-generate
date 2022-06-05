// About history nodes
module history_rules // Most constraints of history nodes, but some constraints are directly with the signatures

open components_sig as components // import all signatures

// In a same level there shouldn't be two history nodes that are of the same type (deep/shallow).
pred noDuplicateTypeHistoryNodes{
        all r1: Regions | lone (ShallowHistoryNodes & r1.contains)
                and lone (DeepHistoryNodes & r1.contains)
        all h1: HierarchicalStates | lone (ShallowHistoryNodes & h1.contains)
                and lone (DeepHistoryNodes & h1.contains)
}

pred noTransitionsBetweenHistoryNodesInSameLevel{
        all h1: HierarchicalStates, disj hn1, hn2: HistoryNodes & h1.contains |
                hn1 not in (Flows <: from).hn2.to
        all r1: Regions, disj hn1, hn2: HistoryNodes & r1.contains |
                hn1 not in (Flows <: from).hn2.to
}

fact{
        // A history should be directed to a same or a deeper level which must contains at least one valid state for history to return, but definitely not to a level further outside, and should never be reached from (somewhere, possibly nested) inside their own compounds excluding start states
        all hs1: HierarchicalStates, h1: HistoryNodes & hs1.contains |
                let n1 = nodesInThisAndDeeper[hs1] |
                {
                        (Flows <: from).h1.to in n1 // A history should be directed to a same or a deeper level
                        some (hs1.contains & States) // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
                        h1 not in (Flows <: from).(n1 - (StartNodes & hs1.contains)).to // History should never be reached from (somewhere, possibly nested) inside their own composite states excluding start nodes
                        h1 in (Flows <: from).(StartNodes & hs1.contains).to implies some (Flows <: from).h1 // If a start node in the same level points to a history node, that history node should probably have an outgoing flow.
                }
        all r1: Regions, h1: HistoryNodes & r1.contains |
                let n1 = nodesInThisAndDeeper[r1] |
                {
                        (Flows <: from).h1.to in n1 // A history should be directed to a same or a deeper level
                        some (r1.contains & States) // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
                        h1 not in (Flows <: from).(n1 - (StartNodes & r1.contains)).to // History should never be reached from (somewhere, possibly nested) inside their own regions excluding start nodes
                        h1 in (Flows <: from).(StartNodes & r1.contains).to implies some (Flows <: from).h1 // If a start node in the same level points to a history node, that history node should probably have an outgoing flow.
                }

        noDuplicateTypeHistoryNodes // In composite states and regions, there should be no history nodes with same types
        noTransitionsBetweenHistoryNodesInSameLevel // A history node should rather not point to another history node in the same level.
}
