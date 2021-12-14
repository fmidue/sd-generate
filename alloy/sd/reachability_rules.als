// About reachability
module reachability_rules // Most constraints of reachability, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

// Each composite state has at least one entry
pred atLeastOneEntryToCompositeStates{
	all c1: CompositeStates | 
		let n1 = allNodesInThisAndDeeper[c1], n2 = Nodes - c1 - n1 |
			c1 in n2.flow[Triggers] or // Standard entry
			some (n1 & n2.flow[Triggers]) // Direct, history or fork entry 
}

pred approximateReachability{
	// Each normal/fork/join/history/end state has at least one incoming arrow (from a start state or somewhere else) 
	all n1: (Nodes - StartStates - CompositeStates) | n1 in (Nodes - n1).flow[Triggers]
	one s1: StartStates | s1 not in allContainedNodes // Outside all hierarchical states and regions, there is exactly one start state
	atLeastOneEntryToCompositeStates
}

pred setStartStatesFlag{
	all s1: StartStates | let h1 = CompositeStates - allContainedNodes |
	({	
		one h1
		no (Nodes - s1 - h1 - allContainedNodes)
		h1 = s1.flow[EmptyTrigger] 
	} or some h2: HierarchicalStates |
	{	
		h2.contains = s1 + EndStates
		s1.flow[EmptyTrigger] = getEndStates[h2]	
	}) implies s1.flag = 1 else s1.flag = 0	
}

				
fact{
	setStartStatesFlag
	approximateReachability
	// If there are history entries without default leaving transition, there must be a start state, because history nodes have neither record and a default leaving transitionat at the first entry
	all h1: HierarchicalStates, h2: HistoryNodes | let n1 = h1.contains | 
		no h2.flow and h2 in (n1 & (Nodes - h1 - n1)).flow[Triggers] implies one getStartStates[h1]
	all r1: Regions, h1: HistoryNodes | let n1 = r1.contains | 
		no h1.flow and h1 in (n1 & (Nodes - n1)).flow[Triggers] implies one getStartStates[r1]
	// If a composite state has regions and there are direct entries to one of the regions(except fork nodes), other regions must have start states
	all disj r1, r2: Regions, rs1: RegionsStates | 
		let n1 = allNodesInThisAndDeeper[r1] | 
			(r1 + r2) in rs1.contains and 
			some (n1 & (Nodes - rs1 - n1 - ForkNodes).flow[Triggers]) implies
				one getStartStates[r2]
	// If a composite without regions has a standard entry, there must be a start state in it.
	all h1: HierarchicalStates | 
		h1 in Nodes.flow[Triggers] implies one getStartStates[h1]
	// If a composite with regions has a standard entry, there must be a start state in each region.
	all rs1: RegionsStates, r1:Regions | 
		r1 in rs1.contains and rs1 in Nodes.flow[Triggers] implies one getStartStates[r1]
}
