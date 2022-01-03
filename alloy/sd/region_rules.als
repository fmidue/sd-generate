// About regions and region states
module region_rules // Most constraints of regions and region states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

// There are no arrows that originate in A and lead to B, if A and B are (in) different regions of a common composite regions state.
pred noCrossing [r1, r2: Regions]{
	disj [nodesInThisAndDeeper[r2], (Flows <: from).(nodesInThisAndDeeper[r1]).to]
	disj [nodesInThisAndDeeper[r1], (Flows <: from).(nodesInThisAndDeeper[r2]).to]
}

fact{
	all rs1: RegionsStates, disj r1, r2: Regions & rs1.contains | noCrossing [r1, r2] // In a same region state, states in different region states can't be transited to each other
}

