// About names of states (and of regions etc.)
module name_rules // Most constraints of names, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

// Entities which are "neighbours" (in the sense of living directly side by side in the same compound or region, but not in two parallel regions of the same compound or such), they must not have the same name. (ignored empty names)
pred noSameTriggersInSameLevels{
	all disj s1, s2: States, h1: HierarchicalStates | 
		(s1 + s2) in h1.contains implies disj[s1.name, s2.name] // In a composite state without regions, no states have same names
	all disj s1, s2: States, r1: Regions | 
		(s1 + s2) in r1.contains  implies disj[s1.name, s2.name] // In a region, no states have same names
	all disj s1, s2: States | 
		(s1 + s2) not in allContainedNodes implies disj[s1.name, s2.name] // In the outermost level, no states have same names
	all disj r1, r2: Regions, rs1: RegionsStates |
		(r1 + r2) in rs1.contains implies disj[r1.name, r2.name] // In a composite state, no regions have same names
}

// In a compound or region, the name of the outermost level must not be repeated anywhere deeper inside. (ignored empty names)s
pred outermostLevelsTriggersNotInDeeperLevels{
	all h1: HierarchicalStates, s1: States | 
		s1 in allNodesInThisAndDeeper[h1] implies disj [h1.name, s1.name]  // For all states deeper inside a composite state without regions
	all r1: Regions, s1: States | 
		s1 in allNodesInThisAndDeeper[r1] implies disj [r1.name, s1.name] // For all states deeper inside a region
	all h1: HierarchicalStates, r1:Regions | 
		r1 in allRegionsInThisAndDeeper[h1] implies disj [h1.name, r1.name] // For all regions deeper inside a composite state without regions
	all r1, r2: Regions | 
		r2 in allRegionsInThisAndDeeper[r1] implies disj [r1.name, r2.name] // For all regions deeper inside a region
}

// About names of states (and of regions etc.)
fact{
	noSameTriggersInSameLevels
	outermostLevelsTriggersNotInDeeperLevels
}
