// About reachability
module reachability_rules // Most constraints of reachability, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	// Each normal/fork/join/history/end state has at least one incoming arrow (from a start state or somewhere else) 
	all n1: (Node - StartState - CompositeStateWithoutRegion - CompositeStateWithRegion) | n1 in (Node - n1).flowto_triggerwith[Trigger]

	// Each composite state has at least one entry
	// If no direct entry and history entry, there must be standard entry
	all c1: CompositeStateWithoutRegion | let n1 = getAllNodeInSameAndDeeperLevel[c1], n2 = Node - c1 - n1 | n1 & n2.flowto_triggerwith[Trigger] = none => c1 in n2.flowto_triggerwith[Trigger]	
	all c1: CompositeStateWithRegion, r1:Region | let n1 = getAllNodeInSameAndDeeperLevel[c1.inner], n2 = Node - c1 - n1 | r1 in c1.inner && n1 & n2.flowto_triggerwith[Trigger] = none => c1 in n2.flowto_triggerwith[Trigger]
	// If only history entry, there must be a start state, because history nodes have no record at the first entry
	all c1: CompositeStateWithoutRegion | let n1 = c1.contains | n1 & (Node - c1 - n1).flowto_triggerwith[Trigger] in History => #(StartState & n1) = 1
	all r1: Region | let n1 = r1.contains | n1 & (Node - n1).flowto_triggerwith[Trigger] in History => #(StartState & n1) = 1
	// If a composite state has regions and there are direct entries to one of the regions(except fork nodes), other regions must have start states
	all disj r1, r2: Region, c1: CompositeStateWithRegion | let n1 = getAllNodeInSameAndDeeperLevel[r1] | (r1 + r2) in c1.inner && n1 & (Node - c1 - n1 - ForkNode).flowto_triggerwith[Trigger] != none => StartState & r2.contains != none
	// If a composite without regions has a standard entry, there must be a start state in it.
	all c1: CompositeStateWithoutRegion | c1 in Node.flowto_triggerwith[Trigger] => #(StartState & c1.contains) = 1
	// If a composite with regions has a standard entry, there must be a start state in each region.
	all c1: CompositeStateWithRegion, r1:Region | r1 in c1.inner && c1 in Node.flowto_triggerwith[Trigger] => #(StartState & r1.contains) = 1 
}
