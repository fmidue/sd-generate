// About fork/join nodes
module node_rules // Most constraints of fork and join nodes, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

// No duplicate fork/join nodes
pred noDuplicateNodes{
	// No duplicate fork nodes
	no disj f1, f2: ForkNodes |
	{ 
		f1.(~to :> Flows).from = f2.(~to :> Flows).from
		and f1.(~from :> Flows).to = f2.(~from :> Flows).to 
		and f1.~from.label = f2.~from.label

	}
	// No duplicate join nodes
	no disj j1, j2: JoinNodes | 
	{	
		j1.(~to :> Flows).from = j2.(~to :> Flows).from
		and j1.(~from :> Flows).to = j2.(~from :> Flows).to 
		and j1.~from.label = j2.~from.label
	} 	
}

// If two (or more) arrows leave the same such fork node, they must go to distinct parallel regions.
pred forkNodesGoToDistinctParalleRegions{
	all f1: ForkNodes | f1.(~from :> Flows).to in Regions.contains
	all f1: ForkNodes, r1: Regions, disj n1, n2: Nodes | 
		(n1 + n2) in r1.contains and n1 in f1.(~from :> Flows).to
			implies n2 not in f1.(~from :> Flows).to
}

// If two (or more) arrows enter the same such join node, they must come from distinct parallel regions.
pred joinNodesComeFromDistinctParalleRegions{
	all j1: JoinNodes | not lone n1: Nodes | n1 in Regions.contains and j1 in n1.(~from :> Flows).to // Each join node has at least two coming arrows from regions.
	all j1: JoinNodes, r1: Regions, disj n1, n2: Nodes | 
		(n1 + n2) in r1.contains and j1 in n1.(~from :> Flows).to 
			implies j1 not in n2.(~from :> Flows).to
}

fact{
	all n1: ForkNodes + JoinNodes |
		(n1 in StartNodes.(~from :> Flows).to implies n1.~from.label = EmptyTrigger) // If such a node(frok and join) is reached from a start state, it is not left by an arrow with non-empty transition label.
		and (n1.~to.label = EmptyTrigger or n1.~from.label = EmptyTrigger) // No such node is both entered and left by arrows with non-empty transition label.、
	forkNodesGoToDistinctParalleRegions
	joinNodesComeFromDistinctParalleRegions
	noDuplicateNodes
}
