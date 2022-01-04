// About fork/join nodes
module node_rules // Most constraints of fork and join nodes, but some constraints are directly with the signatures

open components_sig as components // import all signatures

// If two (or more) arrows leave the same such fork node, they must go to distinct parallel regions.
pred forkNodesGoToDistinctParalleRegions{
	all f1: ForkNodes | one r1: RegionsStates | (Flows <: from).f1.to in r1.contains.contains
	all f1: ForkNodes, r1: Regions, disj n1, n2: r1.contains |
		n1 in (Flows <: from).f1.to implies n2 not in (Flows <: from).f1.to
}

// If two (or more) arrows enter the same such join node, they must come from distinct parallel regions.
pred joinNodesComeFromDistinctParalleRegions{
	all j1: JoinNodes | one r1: RegionsStates | j1 in (Flows <: from).(r1.contains.contains).to
	all j1: JoinNodes, r1: Regions, disj n1, n2: r1.contains |
		j1 in (Flows <: from).n1.to implies j1 not in (Flows <: from).n2.to
}

fact{
	all n1: (ForkNodes + JoinNodes) & (Flows <: from).StartNodes.to |
		from.n1.label = EmptyTrigger // If such a node(frok and join) is reached from a start state, it is not left by an arrow with non-empty transition label.
	all n1: (ForkNodes + JoinNodes) |
		to.n1.label = EmptyTrigger or from.n1.label = EmptyTrigger // No such node is both entered and left by arrows with non-empty transition label.
	forkNodesGoToDistinctParalleRegions
	joinNodesComeFromDistinctParalleRegions
}
