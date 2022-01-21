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
        // A history should be directed to a same or a deeper level which must contains at least one valid state for history to return, but definitely not to a level further outside, and should never be reached from (somewhere, possibly nested) inside their own compounds excluding start states
        all hs1: HierarchicalStates, h1: HistoryNodes & hs1.contains |
                let n1 = nodesInThisAndDeeper[hs1] |
                {
                        from.h1.to in n1 // A history should be directed to a same or a deeper level
                        some (hs1.contains & States) // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
                        h1 not in from.(n1 - (StartNodes & hs1.contains)).to // History should never be reached from (somewhere, possibly nested) inside their own composite states excluding start nodes
                }
        all r1: Regions, h1: HistoryNodes & r1.contains |
                let n1 = nodesInThisAndDeeper[r1] |
                {
                        from.h1.to in n1 // A history should be directed to a same or a deeper level
                        some (r1.contains & States) // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
                        h1 not in from.(n1 - (StartNodes & r1.contains)).to // History should never be reached from (somewhere, possibly nested) inside their own regions excluding start nodes
                }
        atMostOneDeepAndShallowHistoryNodes // In composite states and regions, there is at most one shallow history and at most one deep history
}
