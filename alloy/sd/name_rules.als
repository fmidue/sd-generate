// About names of states (and of regions etc.)
module name_rules // Most constraints of names, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

// About names of states (and of regions etc.)
fact{
	// Entities which are "neighbours" (in the sense of living directly side by side in the same compound or region, but not in two parallel regions of the same compound or such), they must not have the same name. (ignored empty names)
	all disj s1, s2: State, h1: HierarchicalState | (s1 + s2) in h1.h_contains && some (s1.named + s2.named)  => s1.named != s2.named // In a composite state without regions, no states have same names
	all disj s1, s2: State, r1: Region | (s1 + s2) in r1.r_contains && some (s1.named + s2.named) => s1.named != s2.named // In a region, no states have same names
	all disj s1, s2: State | (s1 + s2) not in (Region.r_contains + HierarchicalState.h_contains) && some (s1.named + s2.named) => s1.named != s2.named // In the outermost level, no states have same names
	all disj r1, r2: Region, c1: RegionsState | r1 in c1.inner && r2 in c1.inner && some (r1.named + r2.named) => r1.named != r2.named // In a composite state, no regions have same names

	// In a compound or region, the name of the outermost level must not be repeated anywhere deeper inside. (ignored empty names)
	all h1: HierarchicalState, s1: State | s1 in getAllNodeInSameAndDeeperLevel[h1] => disj [h1.named, s1.named]  // For all states deeper inside a composite state without regions
	all r1: Region, s1: State | s1 in getAllNodeInSameAndDeeperLevel[r1] => disj [r1.named, s1.named] // For all states deeper inside a region
	all h1: HierarchicalState, r1:Region | r1 in getAllRegionInSameAndDeeperLevel[h1.h_contains] => disj [h1.named, r1.named] // For all regions deeper inside a composite state without regions
	all r1, r2: Region | r2 in getAllRegionInSameAndDeeperLevel[r1] => disj [r1.named, r2.named] // For all regions deeper inside a region
}
