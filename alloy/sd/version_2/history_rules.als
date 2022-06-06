// About history nodes
module history_rules // Most constraints of history nodes, but some constraints are directly with the signatures

open components_sig as components // import all signatures

// In a same level there shouldn't be two history nodes that are of the same type (deep/shallow) and either both have no outgoing flow or both have exactly the same outgoing flow.
pred noDuplicateTypeHistoryNodes{
        all r1: Regions, disj sh1, sh2: ShallowHistoryNodes & r1.contains |
                not (from.sh1.to = from.sh2.to)
        all h1: HierarchicalStates, disj sh1, sh2: ShallowHistoryNodes & h1.contains |
                not (from.sh1.to = from.sh2.to)
        all r1: Regions, disj dh1, dh2: DeepHistoryNodes & r1.contains |
                not (from.dh1.to = from.dh2.to)
        all h1: HierarchicalStates, disj dh1, dh2: DeepHistoryNodes & h1.contains |
                not (from.dh1.to = from.dh2.to)
}

pred noTransitionsBetweenHistoryNodesInSameLevel{
        all h1: HierarchicalStates, disj hn1, hn2: HistoryNodes & h1.contains |
                hn1 not in from.hn2.to
        all r1: Regions, disj hn1, hn2: HistoryNodes & r1.contains |
                hn1 not in from.hn2.to
}

fact{
        // A history should be directed to a same or a deeper level which must contains at least one valid state for history to return, but definitely not to a level further outside, and should never be reached from (somewhere, possibly nested) inside their own compounds excluding start states
        all hs1: HierarchicalStates, h1: HistoryNodes & hs1.contains |
                let n1 = nodesInThisAndDeeper[hs1] |
                {
                        from.h1.to in n1 // A history should be directed to a same or a deeper level
                        some (hs1.contains & States) // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
                        h1 not in from.(n1 - (StartNodes & hs1.contains)).to // History should never be reached from (somewhere, possibly nested) inside their own composite states excluding start nodes
                        h1 in from.(StartNodes & hs1.contains).to implies some from.h1 // If a start node in the same level points to a history node, that history node should probably have an outgoing flow.
                }
        all r1: Regions, h1: HistoryNodes & r1.contains |
                let n1 = nodesInThisAndDeeper[r1] |
                {
                        from.h1.to in n1 // A history should be directed to a same or a deeper level
                        some (r1.contains & States) // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
                        h1 not in from.(n1 - (StartNodes & r1.contains)).to // History should never be reached from (somewhere, possibly nested) inside their own regions excluding start nodes
                        h1 in from.(StartNodes & r1.contains).to implies some from.h1 // If a start node in the same level points to a history node, that history node should probably have an outgoing flow.
                }

        noDuplicateTypeHistoryNodes // In composite states and regions, there should be no history nodes with same types
        noTransitionsBetweenHistoryNodesInSameLevel // A history node should rather not point to another history node in the same level.
}
