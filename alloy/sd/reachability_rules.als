// About reachability
module reachability_rules // Most constraints of reachability, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

// Each composite state has at least one entry
pred atLeastOneEntryToCompositeStates{
	all h1: HierarchicalStates | let n1 = getAllNodesInSameAndDeeperLevel[h1], n2 = Nodes - h1 - n1 | 
		h1 in n2.flowto_triggerwith[Names] || some (n1 & n2.flowto_triggerwith[Names]) // Standard, direct or history entry
	all rs1: RegionsStates | let n1 = getAllNodesInSameAndDeeperLevel[rs1.inner], n2 = Nodes - rs1 - n1 | 
		rs1 in n2.flowto_triggerwith[Names] //Standard entry
		|| some (n1 & n2.flowto_triggerwith[Names]) // Direct or history entry
		|| some f1: ForkNodes | (f1 in n2.flowto_triggerwith[Names] && some (n1 & f1.flowto_triggerwith[Names])) // Fork entry
}

fact{
	// Each normal/fork/join/history/end state has at least one incoming arrow (from a start state or somewhere else) 
	all n1: (Nodes - StartStates - CompositeStates) | n1 in (Nodes - n1).flowto_triggerwith[Names]
	atLeastOneEntryToCompositeStates // Each composite state has at least one entry
	// If there are history entries without default leaving transition, there must be a start state, because history nodes have neither record and a default leaving transitionat at the first entry
	all h1: HierarchicalStates, h2: HistoryNodes | let n1 = h1.h_contains | 
		no h2.flowto_triggerwith && h2 in n1 & (Nodes - h1 - n1).flowto_triggerwith[Names] 
			=> one (StartStates & n1)
	all r1: Regions, h1: HistoryNodes | let n1 = r1.r_contains | 
		no h1.flowto_triggerwith && h1 in n1 & (Nodes - n1).flowto_triggerwith[Names] 
			=> one (StartStates & n1)
	// If a composite state has regions and there are direct entries to one of the regions(except fork nodes), other regions must have start states
	all disj r1, r2: Regions, rs1: RegionsStates | let n1 = getAllNodesInSameAndDeeperLevel[r1] | 
		(r1 + r2) in rs1.inner && some (n1 & (Nodes - rs1 - n1 - ForkNodes).flowto_triggerwith[Names]) 
			=> one (StartStates & r2.r_contains)
	// If a composite without regions has a standard entry, there must be a start state in it.
	all h1: HierarchicalStates | h1 in Nodes.flowto_triggerwith[Names] 
		=> one (StartStates & h1.h_contains)
	// If a composite with regions has a standard entry, there must be a start state in each region.
	all rs1: RegionsStates, r1:Regions | 
		r1 in rs1.inner && rs1 in Nodes.flowto_triggerwith[Names] 
			=> one (StartStates & r1.r_contains)
}
