// About fork/join nodes
module node_rules // Most constraints of fork and join nodes, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

// No duplicate fork/join nodes
pred noDuplicateNodes{
	// No duplicate fork nodes
	no disj f1, f2: ForkNodes, n1: Nodes, t1, t2: Triggers | 
		(f1 + f2) in n1.flow[t1] and f1.flow[t2] = f2.flow[t2]
	// No duplicate join nodes
	no disj j1, j2: JoinNodes, disj n1, n2: Nodes, t1, t2: Triggers | 
	{	
		(j1 + j2) in n1.flow[t1] 
		(j1 + j2) in n2.flow[t1] 
		j1.flow[t2] = j2.flow[t2]
	} 	
}

// If two (or more) arrows leave the same such fork node, they must go to distinct parallel regions.
pred forkNodesGoToDistinctParalleRegions{
	all f1: ForkNodes | f1.flow[Triggers] in Regions.contains
	all f1: ForkNodes, r1: Regions, disj n1, n2: Nodes | 
		(n1 + n2) in r1.contains and n1 in f1.flow[Triggers] implies 
			n2 not in f1.flow[Triggers]
}

// If two (or more) arrows enter the same such join node, they must come from distinct parallel regions.
pred joinNodesComeFromDistinctParalleRegions{
	all j1: JoinNodes | j1 in Regions.contains.flow[Triggers]
	all j1: JoinNodes, r1: Regions, disj n1, n2: Nodes | 
		(n1 + n2) in r1.contains and j1 in n1.flow[Triggers] implies 
			j1 not in n2.flow[Triggers]
}

fact{
	all f1: ForkNodes | one n1: Nodes, t1: Triggers | f1 in n1.flow[t1] // Each fork node has only one entering arrow (from a start state or from elsewhere).
	all disj t1, t2 : Triggers | no (JoinNodes & Nodes.flow[t1] & Nodes.flow[t2]) // For join nodes, comming transitions should all have same conditions
	forkNodesGoToDistinctParalleRegions
	joinNodesComeFromDistinctParalleRegions
	all n1: ForkNodes + JoinNodes |
		n1 in StartStates.flow[EmptyTrigger] implies no n1.flow[TriggerNames] // If such a node(frok and join) is reached from a start state, it is not left by an arrow with non-empty transition label.
	all n1: ForkNodes + JoinNodes, t1, t2: Triggers | 
		n1 in Nodes.flow[t1] and some n1.flow[t2] implies 
			t1 = EmptyTrigger or t2 = EmptyTrigger // No such node is both entered and left by arrows with non-empty transition label.
	noDuplicateNodes
}
