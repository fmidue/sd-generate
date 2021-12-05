// About fork/join nodes
module node_rules // Most constraints of fork and join nodes, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	all f1: ForkNode | one n1: Node, t1: Trigger | f1 in n1.flowto_triggerwith[t1] // Each fork node has only one entering arrow (from a start state or from elsewhere).
	all j1: JoinNode, disj t1, t2: Trigger | j1 in Node.flowto_triggerwith[t1] => j1 not in Node.flowto_triggerwith[t2] // For join nodes, comming transitions should all have same conditions
	
	// If two (or more) arrows leave the same such fork node, they must go to distinct parallel regions.
	all f1: ForkNode | f1.flowto_triggerwith[Trigger] in Region.contains
	all f1: ForkNode, c1: CompositeState, r1: Region | f1.flowto_triggerwith[Trigger] & r1.contains != none && r1 in c1.inner => #(f1.flowto_triggerwith[Trigger] & r1.contains) = 1 && #f1.flowto_triggerwith = #c1.inner

	// If two (or more) arrows enter the same such join node, they must come from distinct parallel regions.
	all j1: JoinNode | j1 in Region.contains.flowto_triggerwith[Trigger]
	all j1: JoinNode, r1: Region, disj s1, s2: State | (s1 + s2) in r1.contains && j1 in s1.flowto_triggerwith[Trigger] => j1 not in s2.flowto_triggerwith[Trigger]
	all j1: JoinNode, c1: CompositeState, r1: Region | r1 in c1.inner && j1 in c1.inner.contains.flowto_triggerwith[Trigger] => j1 in r1.contains.flowto_triggerwith[Trigger]
	
	all n1: ForkNode + JoinNode, t1: Trigger | n1 in StartState.flowto_triggerwith[t1] => n1.flowto_triggerwith[t1] = none // If such a node(frok and join) is reached from a start state, it is not left by an arrow with non-empty transition label.
	all n1: ForkNode + JoinNode, t1, t2: Trigger | n1 in Node.flowto_triggerwith[t1] && n1.flowto_triggerwith[t2] != none => t1.notated = none || t2.notated = none // No such node is both entered and left by arrows with non-empty transition label.
	all disj n1, n2: ForkNode + JoinNode, n3:Node, t1, t2: Trigger | (n1 + n2) not in n3.flowto_triggerwith[t1] || n1.flowto_triggerwith[t2] != n2.flowto_triggerwith[t2] // No duplicate fork/join nodes
}
