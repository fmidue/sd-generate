// About names of states (and of regions etc.)
module name_rules // Most constraints of names, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

// About names of states (and of regions etc.)
fact{
	// Entities which are "neighbours" (in the sense of living directly side by side in the same compound or region, but not in two parallel regions of the same compound or such), they must not have the same name. (ignored empty names)
	all disj s1, s2: States, h1: HierarchicalStates | 
		(s1 + s2) in h1.h_contains && some (s1.named + s2.named)  
			=> s1.named != s2.named // In a composite state without regions, no states have same names
	all disj s1, s2: States, r1: Regions | 
		(s1 + s2) in r1.r_contains && some (s1.named + s2.named) 
			=> s1.named != s2.named // In a region, no states have same names
	all disj s1, s2: States | 
		(s1 + s2) not in (Regions.r_contains + HierarchicalStates.h_contains) 
		&& some (s1.named + s2.named) => s1.named != s2.named // In the outermost level, no states have same names
	all disj r1, r2: Regions, c1: RegionsStates |
		r1 in c1.inner && r2 in c1.inner && some (r1.named + r2.named) 
			=> r1.named != r2.named // In a composite state, no regions have same names

	// In a compound or region, the name of the outermost level must not be repeated anywhere deeper inside. (ignored empty names)
	all h1: HierarchicalStates, s1: States | 
		s1 in getAllNodesInSameAndDeeperLevel[h1] => disj [h1.named, s1.named]  // For all states deeper inside a composite state without regions
	all r1: Regions, s1: States | 
		s1 in getAllNodesInSameAndDeeperLevel[r1] => disj [r1.named, s1.named] // For all states deeper inside a region
	all h1: HierarchicalStates, r1:Regions | 
		r1 in getAllRegionsInSameAndDeeperLevel[h1.h_contains] => disj [h1.named, r1.named] // For all regions deeper inside a composite state without regions
	all r1, r2: Regions | 
		r2 in getAllRegionsInSameAndDeeperLevel[r1] => disj [r1.named, r2.named] // For all regions deeper inside a region
}
