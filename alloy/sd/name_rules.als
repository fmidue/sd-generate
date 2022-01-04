// About names of states (and of regions etc.)
module name_rules // Most constraints of names, but some constraints are directly with the signatures

open components_sig as components // import all signatures

// Entities which are "neighbours" (in the sense of living directly side by side in the same compound or region, but not in two parallel regions of the same compound or such), they must not have the same name. (ignored empty names)
pred noSameTriggersInSameLevels{
	all h1: HierarchicalStates, disj s1, s2: States & h1.contains | disj[s1.name, s2.name] // In a composite state without regions, no states have same names
	all r1: Regions, disj s1, s2: States & r1.contains | disj[s1.name, s2.name] // In a region, no states have same names
	all disj s1, s2: States - allContainedNodes | disj[s1.name, s2.name] // In the outermost level, no states have same names
	all rs1: RegionsStates, disj r1, r2: rs1.contains | disj[r1.name, r2.name] // In a composite state, no regions have same names
}

// In a compound or region, the name of the outermost level must not be repeated anywhere deeper inside. (ignored empty names)
pred outermostLevelsTriggersNotInDeeperLevels{
	all h1: HierarchicalStates, s1: States & nodesInThisAndDeeper[h1] | disj[h1.name, s1.name]  // For all states deeper inside a composite state without regions
	all r1: Regions, s1: States & nodesInThisAndDeeper[r1] | disj[r1.name, s1.name] // For all states deeper inside a region
	all h1: HierarchicalStates, r1: regionsInThisAndDeeper[h1] | disj[h1.name, r1.name] // For all regions deeper inside a composite state without regions
	all r1: Regions, r2: regionsInThisAndDeeper[r1] | disj[r1.name, r2.name] // For all regions deeper inside a region
}

// About names of states (and of regions etc.)
fact{
	noSameTriggersInSameLevels
	outermostLevelsTriggersNotInDeeperLevels
}
