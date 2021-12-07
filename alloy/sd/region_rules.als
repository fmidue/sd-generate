// About regions and region states
module region_rules // Most constraints of regions and region states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

// There are no arrows that originate in A and lead to B, if A and B are (in) different regions of a common composite regions state.
pred noCrossing [r1, r2: Region]{
	getAllNodeInSameAndDeeperLevel[r2] & getAllNodeInSameAndDeeperLevel[r1].flowto_triggerwith[Trigger] = none
	getAllNodeInSameAndDeeperLevel[r1] & getAllNodeInSameAndDeeperLevel[r2].flowto_triggerwith[Trigger] = none
}

fact{
	all r1: Region, disj c1, c2: CompositeStateWithRegion | r1 in c1.inner => r1 not in c2.inner // No same regions are inside different composite states with regions
	all disj r1, r2: Region, c1: CompositeStateWithRegion |  (r1 + r2) in c1.inner => noCrossing [r1, r2] // In a same composite state, states in different region states can't be transited to each other
	all r1: Region, c1: CompositeStateWithRegion | r1 in c1.inner => c1 not in r1.contains // If a region is in a composite state, it can't contain the composite state, otherwise it has a loop relation
}
