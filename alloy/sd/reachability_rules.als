// About reachability
module reachability_rules // Most constraints of reachability, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

// Each composite state has at least one entry, except something like "box", in which all events happen, but it also has a default standard entry from the outermost start node which can be set invisible
pred atLeastOneEntryToCompositeStates{
	all c1: CompositeStates | 
		let n1 = nodesInThisAndDeeper[c1], n2 = Nodes - c1 - n1 |
			c1 in n2.(~from :> Flows).to // Standard entry
			or some (n1 & n2.(~from :> Flows).to) // Direct, history or fork entry 
}

// It implements only approximate reachability
pred approximateReachability{
	no derived
	// Each normal/fork/join/history/end state has at least one incoming arrow (from a start state or somewhere else) 
	all n1: (Nodes - StartNodes - CompositeStates) | n1 in (Nodes - n1).(~from :> Flows).to
	one s1: StartNodes | s1 not in allContainedNodes // Outside all hierarchical states and regions, there is exactly one start state
	atLeastOneEntryToCompositeStates
}

// It implements true reachability
pred trueReachability{
	atLeastOneEntryToCompositeStates // It is a necessary condition for "true reachability"
	no disj pf1, pf2: ProtoFlows | pf1.from = pf2.from and pf1.to = pf2.to and pf1.label = pf2.label// no duplicate flows
	(ProtoFlows - Flows) in ProtoFlows.derived
	// The following are predicates to implement "flattening".
	// It flattens flows from composite states to normal states and end nodes
	all pf: ProtoFlows | let sn = StartNodes - (StartNodes & allContainedNodes) |
		(pf.from in CompositeStates and pf.to in (NormalStates + EndNodes))
   			implies (let cs = States & nodesInThis[pf.from] |
    				((all s: cs | one tf: pf.derived | tf.from = s and tf.to = pf.to)
    				and (all tf : pf.derived | tf.from in cs and tf.to = pf.to))) 
			else
		(pf.from in (sn + States) and pf.to in CompositeStates)
   			implies (let cs = (StartNodes & nodesInThis[pf.to]).~from.to |
    				((all s: cs | one tf: pf.derived | tf.from = pf.from and tf.to = s)
    				and (all tf : pf.derived | tf.from = pf.from and tf.to in cs))) 
			else
		// // It flattens flows from all states and the outermost start node to all states in regions, here end nodes are excluded, because coming to an end node means all end. 
		(pf.from in (sn + States) and pf.to in (Regions.contains & States))
			implies (let cs = (nodesInOtherParallelRegions[(Regions <: contains).(pf.to)] & StartNodes).~from.to |
				((all s: cs | one tf: pf.derived | tf.from = pf.from and tf.to = s)
				and (all tf: pf.derived | tf.from = pf.from and tf.to in cs)))
			else
		// It seems that above 3 predicates can constrain nodes except special nodes.
		// It flattens flows from all states and the outermost start node to fork nodes
		(pf.from in (sn + States) and pf.to in ForkNodes)
			implies (let cs = pf.to.~from.to + (nodesInOtherParallelRegions[(Regions <: contains).(pf.to.~from.to)] & StartNodes).~from.to |  
				((all s: cs | one tf: pf.derived | tf.from = pf.from and tf.to = s)
				and (all tf: pf.derived | tf.from = pf.from and tf.to in cs)))
			else
		// It flattens flows from join nodes to all states and the outermost start node
		(pf.from in States & Regions.contains and pf.to in JoinNodes)
			implies (let cs = pf.from | // Here, I omit flattening all states in other parallel regions, because it doesn't have impact on judging reachability, but if it is added, it will increase considerable complexity and recursions will exist
				((all s: cs | one tf: pf.derived | tf.from = s and tf.to = pf.to.~from.to)
				and (all tf: pf.derived | tf.from in cs and tf.to = pf.to.~from.to)))
			else
		// The following 4 predicates flatten flows from states and the outermost start node to history nodes
		// When a history node is in a region and has no default flow (states + the outermost start state -> history nodes in regions and without a default flow)
		(pf.from in (sn + States) and pf.to in HistoryNodes and no pf.to.~from and pf.to in Regions.contains)
			implies (let cs = (StartNodes & ((RegionsStates <: contains).contains.(pf.to)).contains.contains).~from.to |
				((all s: cs | one tf: pf.derived | tf.from = pf.from and tf.to = s)
				and (all tf: pf.derived | tf.from = pf.from and tf.to in cs)))
			else
		// When a history node is in a region and has a default flow (states + the outermost start state -> history nodes in regions and with a default flow)
		(pf.from in (sn + States) and pf.to in HistoryNodes and one pf.to.~from and pf.to in Regions.contains)
			implies (let cs = pf.to.(~from :> Flows).to + nodesInOtherParallelRegions[contains.(pf.to)].~from.to |
				((all s: cs | one tf: pf.derived | tf.from = pf.from and tf.to = s)
				and (all tf: pf.derived | tf.from = pf.from and tf.to in cs)))
			else
		// When a history node is in a hierarchical state and has no default flow (states + the outermost start state -> history nodes in hierarchical states and without a default flow)
		(pf.from in (sn + States) and pf.to in HistoryNodes and no pf.to.~from and pf.to in HierarchicalStates.contains)
			implies (let cs = (StartNodes & (HierarchicalStates <: contains).(pf.to).contains).~from.to |
				((one tf: pf.derived | tf.from = pf.from and tf.to = cs)
				and (all tf: pf.derived | tf.from = pf.from and tf.to = cs)))
			else
		// When a history node is in a hierarchical state and has a default flow (states + the outermost start state -> history nodes in hierarchical states and with a default flow)
		(pf.from in (sn + States) and pf.to in HistoryNodes and one pf.to.~from and pf.to in HierarchicalStates.contains)
			implies ((one tf: pf.derived | tf.from = pf.from and tf.to = pf.to.~from.to)
				and (all tf: pf.derived | tf.from = pf.from and tf.to = pf.to.~from.to))
			else no pf.derived

	(Nodes - StartNodes - CompositeStates) in 
		(StartNodes - (StartNodes & allContainedNodes)).^(~from.to)// Starting from a random nodes
}

// set the flag representing if the start node is invisible
pred setStartNodesFlag{
	all s1: StartNodes | let h1 = CompositeStates - allContainedNodes |
	({	
		one h1
		no (Nodes - s1 - h1 - allContainedNodes)
		h1 = s1.(~from :> Flows).to
	} or some h2: HierarchicalStates |
	{	
		h2.contains = s1 + EndNodes
		s1.(~from :> Flows).to = (EndNodes & h2.contains)	
	}) implies s1.flag = 1 else s1.flag = 0	
}
			
fact{
	setStartNodesFlag
	trueReachability
	// If there are history entries without default leaving transition, there must be a start state, because history nodes have neither record and a default leaving transitionat at the first entry
	all h1: HierarchicalStates, h2: HistoryNodes | let n1 = h1.contains | 
		no h2.(~from :> Flows).to and h2 in (n1 & (Nodes - h1 - n1)).(~from :> Flows).to
			implies one (StartNodes & h1.contains)
	all r1: Regions, h1: HistoryNodes | let n1 = r1.contains | 
		no h1.(~from :> Flows) and h1 in (n1 & (Nodes - n1)).(~from :> Flows).to 
			implies one (StartNodes & r1.contains)
	// If a composite state has regions and there are direct entries to one of the regions(except fork nodes), other regions must have start states
	all disj r1, r2: Regions, rs1: RegionsStates | 
		let n1 = nodesInThisAndDeeper[r1] | 
			(r1 + r2) in rs1.contains  
			and some (n1 & (Nodes - rs1 - n1 - ForkNodes).(~from :> Flows).to) 
				implies one (StartNodes & r2.contains)
	// If a composite state with regions has a fork entry, those parallel regions without the entry from the fork node will contain a start node.
	all r1: Regions, rs1: RegionsStates, f1: ForkNodes | 
		some (f1.(~from :> Flows).to & nodesInThisAndDeeper[rs1]) 
		and r1 in rs1.contains and f1 not in rs1.contains.contains
		and no (f1.(~from :> Flows).to & nodesInThisAndDeeper[r1])
			implies one (StartNodes & r1.contains) 
	// If a composite without regions has a standard entry, there must be a start state in it.
	all h1: HierarchicalStates | 
		h1 in Nodes.(~from :> Flows).to implies one (StartNodes & h1.contains)
	// If a composite with regions has a standard entry, there must be a start state in each region.
	all rs1: RegionsStates, r1:Regions | 
		r1 in rs1.contains and rs1 in Nodes.(~from :> Flows).to
			implies one (StartNodes & r1.contains)
}
