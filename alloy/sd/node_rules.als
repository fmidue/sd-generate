// About fork/join nodes
module node_rules // Most constraints of fork and join nodes, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	all f1: ForkNodes | one n1: Nodes, t1: Names | f1 in n1.flowto_triggerwith[t1] // Each fork node has only one entering arrow (from a start state or from elsewhere).
	all j1: JoinNodes, disj t1, t2: Names | 
		j1 in Nodes.flowto_triggerwith[t1] => j1 not in Nodes.flowto_triggerwith[t2] // For join nodes, comming transitions should all have same conditions
	
	// If two (or more) arrows leave the same such fork node, they must go to distinct parallel regions.
	all f1: ForkNodes | f1.flowto_triggerwith[Names] in Regions.r_contains
	all f1: ForkNodes, rs1: RegionsStates, r1: Regions | 
		r1 in rs1.inner && some (f1.flowto_triggerwith[Names] & rs1.inner.r_contains) 
			=> one (f1.flowto_triggerwith[Names] & r1.r_contains)
	
	// If two (or more) arrows enter the same such join node, they must come from distinct parallel regions.
	all j1: JoinNodes | j1 in Regions.r_contains.flowto_triggerwith[Names]
	all j1: JoinNodes, r1: Regions, disj s1, s2: States | 
		(s1 + s2) in r1.r_contains && j1 in s1.flowto_triggerwith[Names] 
			=> j1 not in s2.flowto_triggerwith[Names]
	all j1: JoinNodes, rs1: RegionsStates, r1: Regions | 
		r1 in rs1.inner && j1 in rs1.inner.r_contains.flowto_triggerwith[Names] 
			=> j1 in r1.r_contains.flowto_triggerwith[Names]
	
	all n1: ForkNodes + JoinNodes |
		n1 in StartStates.flowto_triggerwith[EmptyTriggers] => no n1.flowto_triggerwith[NonEmptyTriggers] // If such a node(frok and join) is reached from a start state, it is not left by an arrow with non-empty transition label.
	all n1: ForkNodes + JoinNodes, t1, t2: Names | 
		n1 in Nodes.flowto_triggerwith[t1] && some n1.flowto_triggerwith[t2] 
			=>  t1 = EmptyTriggers || t2 = EmptyTriggers // No such node is both entered and left by arrows with non-empty transition label.
	// No duplicate fork nodes
	no disj f1, f2: ForkNodes, n1:Nodes, t1, t2: Names | 
		(f1 + f2) in n1.flowto_triggerwith[t1] 
		&& f1.flowto_triggerwith[t2] = f2.flowto_triggerwith[t2]
	// No duplicate join nodes
	no disj j1, j2: JoinNodes, disj n1, n2: Nodes, t1, t2: Names | 
		(j1 + j2) in n1.flowto_triggerwith[t1] 
		&& (j1 + j2) in n2.flowto_triggerwith[t1] 
		&& j1.flowto_triggerwith[t2] = j2.flowto_triggerwith[t2] 	
}
