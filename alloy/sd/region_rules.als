// About regions and region states
module region_rules // Most constraints of regions and region states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

// There are no arrows that originate in A and lead to B, if A and B are (in) different regions of a common composite regions state.
pred noCrossing [r1, r2: Regions]{
	disj [allNodesInThisAndDeeper[r2], allNodesInThisAndDeeper[r1].flow[Triggers]]
	disj [allNodesInThisAndDeeper[r1], allNodesInThisAndDeeper[r2].flow[Triggers]]
}

fact{
	all disj r1, r2: Regions, rs1: RegionsStates |
		(r1 + r2) in rs1.contains implies noCrossing [r1, r2] // In a same region state, states in different region states can't be transited to each other
}

