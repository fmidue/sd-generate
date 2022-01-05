// About reachability
module reachability_rules // Most constraints of reachability, but some constraints are directly with the signatures

open components_sig as components // import all signatures
open trueReachability_v2 as trueReachability // import "trueReachability"

// It implements only approximate reachability
pred approximateReachability{
        no derived
        // Each normal/fork/join/history/end state has at least one incoming arrow (from a start state or somewhere else)
        all n1: (Nodes - StartNodes - CompositeStates) | n1 in (Flows <: from).(Nodes - n1).to
        one (StartNodes - allContainedNodes) // Outside all hierarchical states and regions, there is exactly one start state
        atLeastOneEntryToCompositeStates
}

// set the flag representing if the start node is invisible
pred setStartNodesFlag{
        all s1: StartNodes | let h1 = CompositeStates - allContainedNodes |
        ({
                one h1
                no (Nodes - s1 - h1 - allContainedNodes)
                h1 = (Flows <: from).s1.to
        } or some h2: HierarchicalStates |
        {
                h2.contains = s1 + EndNodes
                (Flows <: from).s1.to = (EndNodes & h2.contains)
        }) implies s1.flag = 1 else s1.flag = 0
}

fact{
        setStartNodesFlag
        trueReachability
        // If there are history entries without default leaving transition, there must be a start state, because history nodes have neither record and a default leaving transitionat at the first entry
        all h1: HierarchicalStates, h2: HistoryNodes | let n1 = h1.contains |
                no (Flows <: from).h2.to and h2 in (Flows <: from).(n1 & (Nodes - h1 - n1)).to
                        implies one (StartNodes & h1.contains)
        all r1: Regions, h1: HistoryNodes | let n1 = r1.contains |
                no (Flows <: from).h1 and h1 in (Flows <: from).(n1 & (Nodes - n1)).to
                        implies one (StartNodes & r1.contains)
        // If a composite state has regions and there are direct entries to one of the regions(except fork nodes), other regions must have start states
        all rs1: RegionsStates, disj r1, r2: rs1.contains |
                let n1 = nodesInThisAndDeeper[r1] |
                        some ((Flows <: from).(n1 & (Nodes - rs1 - n1 - ForkNodes)).to)
                                implies one (StartNodes & r2.contains)
        // If a composite state with regions has a fork entry, those parallel regions without the entry from the fork node will contain a start node.
        all rs1: RegionsStates, r1: rs1.contains, f1: ForkNodes |
                some ((Flows <: from).f1.to & nodesInThisAndDeeper[rs1])
                and f1 not in rs1.contains.contains
                and no ((Flows <: from).f1.to & nodesInThisAndDeeper[r1])
                        implies one (StartNodes & r1.contains)
        // If a composite without regions has a standard entry, there must be a start state in it.
        all h1: HierarchicalStates & (Flows <: from).Nodes.to | one (StartNodes & h1.contains)
        // If a composite with regions has a standard entry, there must be a start state in each region.
        all rs1: RegionsStates & (Flows <: from).Nodes.to, r1: rs1.contains |
                 one (StartNodes & r1.contains)
}
