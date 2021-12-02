// About names of states (and of regions etc.)
module name_rules // most constraints of names, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

//About names of states (and of regions etc.)
fact{
	// Entities which are "neighbours" (in the sense of living directly side by side in the same compound or region, but not in two parallel regions of the same compound or such), they must not have the same name.
	all s1, s2: State, c1: CompositeState | s1 != s2 && (s1 in c1.contains && s2 in c1.contains) => s1.named != s2.named // In a composite state, no states have same names
	all s1, s2: State, r1: Region | s1 != s2 && (s1 in r1.contains && s2 in r1.contains) => s1.named != s2.named // In a region, no states have same names
	all s1, s2: State | s1 != s2 && (s1 not in (Region.contains + CompositeState.contains)) && (s2 not in (Region.contains + CompositeState.contains))  => s1.named != s2.named // In the outermost level, no states have same names
	all r1, r2: Region, c1: CompositeState |  r1 != r2 && r1 in c1.inner && r2 in c1.inner => r1.named != r2.named // In a composite state, no regions have same names

	// In a compound or region, the name of the outermost level must not be repeated anywhere deeper inside.
//	all c1: CompositeState |  c1.named != c1.^contains.named && c1.named != c1.inner.named && c1.named != c1.inner.contains.named
//	all r1: Region | r1.named != r1.contains.named
}
