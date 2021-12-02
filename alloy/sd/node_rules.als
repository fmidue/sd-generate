// About fork/join nodes
module node_rules // most constraints of fork and join nodes, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	all f1: ForkNode, t1, t2: Trigger | f1.flowto_triggerwith[t1] != none && f1.flowto_triggerwith[t2] != none => t1 = t2 // for fork nodes, leaving transitions should all have same conditions or no conditions
	all j1: JoinNode, t1, t2: Trigger | j1.flowfrom_triggerwith[t1] != none && j1.flowfrom_triggerwith[t2] != none => t1 = t2 // for join nodes, comming transitions should all have same conditions or no conditions
	
	// If two (or more) arrows leave the same such fork node, they must go to distinct parallel regions.
	all f1: ForkNode | f1.flowto_triggerwith[Trigger] in Region.contains
	all f1: ForkNode, c1: CompositeState, r1: Region | f1.flowto_triggerwith[Trigger] & r1.contains != none && r1 in c1.inner => #(f1.flowto_triggerwith[Trigger] & r1.contains) = 1 && #f1.flowto_triggerwith = #c1.inner

	// If two (or more) arrows enter the same such join node, they must come from distinct parallel regions.
	all j1: JoinNode | j1.flowfrom_triggerwith[Trigger] in Region.contains
	all  j1: JoinNode, c1: CompositeState, r1: Region | j1.flowfrom_triggerwith[Trigger] in r1.contains && r1 in c1.inner => #(j1.flowfrom_triggerwith[Trigger] & r1.contains) = 1 && #j1.flowfrom_triggerwith = #c1.inner
	
	all n1: ForkNode + JoinNode, t1: Trigger | StartState in n1.flowfrom_triggerwith[t1] => n1.flowto_triggerwith[t1] != none //If such a node(frok and join) is reached from a start state, it is not left by an arrow with non-empty transition label.
	all n1: ForkNode + JoinNode, t1, t2: Trigger | n1.flowfrom_triggerwith[t1] != none && n1.flowfrom_triggerwith[t2] != none => t1.notated = none || t2.notated = none // No such node is both entered and left by arrows with non-empty transition label.
	all n1, n2: Node, t1, t2: Trigger | n1 != n2 => n1.flowfrom_triggerwith[t1] != n2.flowfrom_triggerwith[t1] && n1.flowto_triggerwith[t2] != n2.flowto_triggerwith[t2] // No duplicate nodes
}
