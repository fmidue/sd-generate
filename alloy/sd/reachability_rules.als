// About reachability
module reachability_rules // Most constraints of reachability, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	// Each normal/fork/join/history/end state has at least one incoming arrow (from a start state or somewhere else) 
	all n1: (Node - StartState - CompositeState) | n1 in (Node - n1).flowto_triggerwith[Trigger]

	// Each composite state has at least one entry
	// If no direct entry and history entry, there must be standard entry
	all h1: HierarchicalState | let n1 = getAllNodeInSameAndDeeperLevel[h1], n2 = Node - h1 - n1 | disj [n1, n2.flowto_triggerwith[Trigger]] => h1 in n2.flowto_triggerwith[Trigger]	
	all rs1: RegionsState, r1:Region | let n1 = getAllNodeInSameAndDeeperLevel[rs1.inner], n2 = Node - rs1 - n1 | r1 in rs1.inner && disj [n1, n2.flowto_triggerwith[Trigger]] => rs1 in n2.flowto_triggerwith[Trigger]
	// If only history entry, there must be a start state, because history nodes have no record at the first entry
	all h1: HierarchicalState | let n1 = h1.h_contains | n1 & (Node - h1 - n1).flowto_triggerwith[Trigger] in History => one (StartState & n1)
	all r1: Region | let n1 = r1.r_contains | n1 & (Node - n1).flowto_triggerwith[Trigger] in History => one (StartState & n1)
	// If a composite state has regions and there are direct entries to one of the regions(except fork nodes), other regions must have start states
	all disj r1, r2: Region, rs1: RegionsState | let n1 = getAllNodeInSameAndDeeperLevel[r1] | (r1 + r2) in rs1.inner && some (n1 & (Node - rs1 - n1 - ForkNode).flowto_triggerwith[Trigger]) => one (StartState & r2.r_contains)
	// If a composite without regions has a standard entry, there must be a start state in it.
	all h1: HierarchicalState | h1 in Node.flowto_triggerwith[Trigger] => one (StartState & h1.h_contains)
	// If a composite with regions has a standard entry, there must be a start state in each region.
	all rs1: RegionsState, r1:Region | r1 in rs1.inner && rs1 in Node.flowto_triggerwith[Trigger] => one (StartState & r1.r_contains) 
}
