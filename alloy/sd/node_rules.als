// About fork/join nodes
module node_rules // Most constraints of fork and join nodes, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	all f1: ForkNode | one n1: Node, t1: Name | f1 in n1.flowto_triggerwith[t1] // Each fork node has only one entering arrow (from a start state or from elsewhere).
	all j1: JoinNode, disj t1, t2: Name | 
		j1 in Node.flowto_triggerwith[t1] => j1 not in Node.flowto_triggerwith[t2] // For join nodes, comming transitions should all have same conditions
	
	// If two (or more) arrows leave the same such fork node, they must go to distinct parallel regions.
	all f1: ForkNode | f1.flowto_triggerwith[Name] in Region.r_contains
	all f1: ForkNode, rs1: RegionsState, r1: Region | 
		r1 in rs1.inner && some (f1.flowto_triggerwith[Name] & rs1.inner.r_contains) 
			=> one (f1.flowto_triggerwith[Name] & r1.r_contains)
	
	// If two (or more) arrows enter the same such join node, they must come from distinct parallel regions.
	all j1: JoinNode | j1 in Region.r_contains.flowto_triggerwith[Name]
	all j1: JoinNode, r1: Region, disj s1, s2: State | 
		(s1 + s2) in r1.r_contains && j1 in s1.flowto_triggerwith[Name] 
			=> j1 not in s2.flowto_triggerwith[Name]
	all j1: JoinNode, rs1: RegionsState, r1: Region | 
		r1 in rs1.inner && j1 in rs1.inner.r_contains.flowto_triggerwith[Name] 
			=> j1 in r1.r_contains.flowto_triggerwith[Name]
	
	all n1: ForkNode + JoinNode |
		n1 in StartState.flowto_triggerwith[EmptyTrigger] => no n1.flowto_triggerwith[NonEmptyTrigger] // If such a node(frok and join) is reached from a start state, it is not left by an arrow with non-empty transition label.
	all n1: ForkNode + JoinNode, t1, t2: Name | 
		n1 in Node.flowto_triggerwith[t1] && some n1.flowto_triggerwith[t2] 
			=>  t1 = EmptyTrigger || t2 = EmptyTrigger // No such node is both entered and left by arrows with non-empty transition label.
	// No duplicate fork nodes
	no disj f1, f2: ForkNode, n1:Node, t1, t2: Name | 
		(f1 + f2) in n1.flowto_triggerwith[t1] 
		&& f1.flowto_triggerwith[t2] = f2.flowto_triggerwith[t2]
	// No duplicate join nodes
	no disj j1, j2: JoinNode, disj n1, n2: Node, t1, t2: Name | 
		(j1 + j2) in n1.flowto_triggerwith[t1] 
		&& (j1 + j2) in n2.flowto_triggerwith[t1] 
		&& j1.flowto_triggerwith[t2] = j2.flowto_triggerwith[t2] 	
}
