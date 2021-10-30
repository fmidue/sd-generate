abstract sig Nodes{
//	from,to: Nodes
}
{
//	from != to
}

// a same state can't be in different blocks(regions and layouts)
abstract sig Blocks{
	contains: disj set Nodes
}

// all states are nodes
abstract sig State extends Nodes{

}

// Regions and Layouts are Blocks
abstract sig Region extends Blocks{
	partof: one Layout
}

abstract sig Layout extends Blocks{
	belongs: lone Layout,
	inner: set Region
}

// it is replaced by the definition contains: disj set Nodes
/*
pred notIn [b1, b2: Blocks]{
	b1.contains not in b2.contains
}
*/

// a same state can't be in different blocks(regions and layouts)
fact{
//	all r1, r2: Region | r1 != r2 => notIn[r1, r2]    // it is replaced by the definition contains: disj set Nodes
//	all l1, l2: Layout | l1 != l2 => notIn[l1, l2]     // it is replaced by the definition contains: disj set Nodes
	no l1: Layout | l1 in l1.^belongs // this relation has no loop
	partof = ~inner  //reverse relation
	all l1: Layout | #l1.inner !=1 // Regions divide a state diagram into at least two areas that run in parallel.
}

// if a layout contains regions, then all states are contained by regions directly and in the layout indirectly, so the layout doesn't contain any states directly
fact{
	all s1: State | s1 in Region.contains => s1 not in Layout.contains
	all l1, l2: Layout, s1: State | #l1.inner > 0 => s1 not in l1.contains && l1 not in l2.^belongs
}

//In a same layout, states in different regions can't be transited to each other
/*
fact{
	all r1, r2: Region |  r1.partof = r2.partof => r1.contains.from not in (r2.contains.from + r2.contains.to)
	all r1, r2: Region |  r1.partof = r2.partof => r1.contains.to not in (r2.contains.from + r2.contains.to)
}
*/

//check {}
run {} for 6 but exactly 2 Layout, 2 State, 2 Region
