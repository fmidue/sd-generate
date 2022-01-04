// About fork/join nodes
module node_rules // Most constraints of fork and join nodes, but some constraints are directly with the signatures

open components_sig as components // import all signatures

// If two (or more) arrows leave the same such fork node, they must go to distinct parallel regions.
pred forkNodesGoToDistinctParalleRegions{
	all fn : ForkNodes | one rs : RegionsStates | (Flows <: from).fn.to in nodesInThisAndDeeper[rs]
		and no r : rs.contains, disj f1, f2 : (Flows <: from).fn |
			(f1 + f2).to in nodesInThisAndDeeper[r]
}

// If two (or more) arrows enter the same such join node, they must come from distinct parallel regions.
pred joinNodesComeFromDistinctParalleRegions{
	all jn: JoinNodes | one rs: RegionsStates | (Flows <: to).jn.from in nodesInThisAndDeeper[rs]
		and no r : rs.contains, disj f1, f2 : (Flows <: to).jn |
			(f1 + f2).from in nodesInThisAndDeeper[r]
}

fact{
	all n1: (ForkNodes + JoinNodes) & (Flows <: from).StartNodes.to |
		from.n1.label = EmptyTrigger // If such a node(frok and join) is reached from a start state, it is not left by an arrow with non-empty transition label.
	all n1: (ForkNodes + JoinNodes) |
		to.n1.label = EmptyTrigger or from.n1.label = EmptyTrigger // No such node is both entered and left by arrows with non-empty transition label.
	forkNodesGoToDistinctParalleRegions
	joinNodesComeFromDistinctParalleRegions
}
