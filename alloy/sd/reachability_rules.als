// About reachability
module reachability_rules // Most constraints of reachability, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

// Each composite state has at least one entry, except something like "box", in which all events happen, but it also has a default standard entry from the outermost start node which can be set invisible
pred atLeastOneEntryToCompositeStates{
	all c1: CompositeStates | 
		let n1 = nodesInThisAndDeeper[c1], n2 = Nodes - c1 - n1 |
			c1 in n2.flow[Triggers] // Standard entry
			or some (n1 & n2.flow[Triggers]) // Direct, history or fork entry 
}

pred approximateReachability{
	// Each normal/fork/join/history/end state has at least one incoming arrow (from a start state or somewhere else) 
	all n1: (Nodes - StartNodes - CompositeStates) | n1 in (Nodes - n1).flow[Triggers]
	one s1: StartNodes | s1 not in allContainedNodes // Outside all hierarchical states and regions, there is exactly one start state
	atLeastOneEntryToCompositeStates
}

// set the flag representing if the start node is invisible
pred setStartNodesFlag{
	all s1: StartNodes | let h1 = CompositeStates - allContainedNodes |
	({	
		one h1
		no (Nodes - s1 - h1 - allContainedNodes)
		h1 = s1.flow[EmptyTrigger] 
	} or some h2: HierarchicalStates |
	{	
		h2.contains = s1 + EndNodes
		s1.flow[EmptyTrigger] = (EndNodes & h2.contains)	
	}) implies s1.flag = 1 else s1.flag = 0	
}
			
fact{
	setStartNodesFlag
	approximateReachability
	// If there are history entries without default leaving transition, there must be a start state, because history nodes have neither record and a default leaving transitionat at the first entry
	all h1: HierarchicalStates, h2: HistoryNodes | let n1 = h1.contains | 
		no h2.flow and h2 in (n1 & (Nodes - h1 - n1)).flow[Triggers] 
			implies one (StartNodes & h1.contains)
	all r1: Regions, h1: HistoryNodes | let n1 = r1.contains | 
		no h1.flow and h1 in (n1 & (Nodes - n1)).flow[Triggers] 
			implies one (StartNodes & r1.contains)
	// If a composite state has regions and there are direct entries to one of the regions(except fork nodes), other regions must have start states
	all disj r1, r2: Regions, rs1: RegionsStates | 
		let n1 = nodesInThisAndDeeper[r1] | 
			(r1 + r2) in rs1.contains  
			and some (n1 & (Nodes - rs1 - n1 - ForkNodes).flow[Triggers]) 
				implies one (StartNodes & r2.contains)
	// If a composite state with regions has a fork entry, those parallel regions without the entry from the fork node will contain a start node.
	all r1: Regions, rs1: RegionsStates, f1: ForkNodes | 
		some (f1.flow[Triggers] & nodesInThisAndDeeper[rs1]) 
		and r1 in rs1.contains and f1 not in rs1.contains.contains
		and no (f1.flow[Triggers] & nodesInThisAndDeeper[r1])
			implies one (StartNodes & r1.contains) 
	// If a composite without regions has a standard entry, there must be a start state in it.
	all h1: HierarchicalStates | 
		h1 in Nodes.flow[Triggers] implies one (StartNodes & h1.contains)
	// If a composite with regions has a standard entry, there must be a start state in each region.
	all rs1: RegionsStates, r1:Regions | 
		r1 in rs1.contains and rs1 in Nodes.flow[Triggers] 
			implies one (StartNodes & r1.contains)
}
