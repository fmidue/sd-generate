module uml_state_diagram

open components_sig as components

pred noCrossing [r1, r2: Region]{
	 r2.contains not in r1.contains.flow_triggerwith[Trigger] && r1.contains not in  r2.contains.flow_triggerwith[Trigger]
}


//rules of start states and end states
fact{
	#StartState > 0 // at least one start state
	all s1: StartState, t1: Trigger | s1.flow_triggerwith[t1] != none || s1 in Node.flowfrom_triggerwith[t1] => t1.notated = none // Start states are not left by an arrow with non-empty transition label
	all s1: StartState| s1 not in State.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger] // no transitions to start states
			    && s1 not in JoinNode.flowfrom_triggerwith[Trigger] // no transitions between start states and join nodes

	//Start states are only left by arrows pointing to something in their own compound state or deeper.
	all s1: StartState, c1: CompositeState | s1 in (c1.contains + c1.inner.contains) => s1.flow_triggerwith[Trigger] in (c1.contains + c1.inner.contains) 
	
	all r1: Region, s1: StartState | s1 in r1.contains => #s1 < 2 //In regions, there is at most one start state.
	all c1: CompositeState, s1: StartState | s1 in c1.contains => #s1 < 2 //In composite states, there is at most one start state.
	all s1: StartState | s1 not in (CompositeState.contains + Region.contains) => #s1 < 2 //Outside composite states and regions, there is also at most one start state.
	all r1: Region, e1: EndState | e1 in r1.contains => #e1 < 2 //In regions, there is at most one end state.
	all c1: CompositeState, e1: EndState | e1 in c1.contains => #e1 < 2 //In composite states, there is at most one end state.
	all e1: StartState | e1 not in (CompositeState.contains + Region.contains) => #e1 < 2 //Outside composite states and regions, there is also at most one end state.
	all e1: EndState | e1 in (State.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger]) // If end states exists, there must be transitions to them
			    && e1 not in Node.flowfrom_triggerwith[Trigger] // No leaving transitions(including to nodes), only coming transitions,
			    && e1 not in ForkNode.flowto_triggerwith[Trigger] // No transitions between end states and fork nodes
}
//rules of composite states
fact{
	no c1: CompositeState | c1 in c1.^contains // this relation has no loop
	partof = ~inner  //reverse relation
}

//rules of regions
fact{
	all r1, r2: Region |  r1.partof = r2.partof => noCrossing [r1, r2] //In a same composite state, states in different region states can't be transited to each other
	all r1: Region, c1: CompositeState | r1 in c1.inner => c1 not in r1.contains // if a region is in a composite state, it can't contain the composite state, otherwise it has a loop relation
}

//rules of fork nodes and join nodes
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

//rules of History
fact{
	// A history should be directed to a same or a deeper level, but definitely not to a level further outside
	all h1: History, c1: CompositeState |  #c1.inner = 0 && h1 in (c1.s_possess + c1.d_possess) => h1.flowto_triggerwith[Trigger] = none || h1.flowto_triggerwith[Trigger] in (c1.^contains + c1.^contains.inner.contains.*contains)
	all h1: History, c1: CompositeState |  #c1.inner > 0 && h1 in (c1.inner.s_possess + c1.inner.d_possess) => h1.flowto_triggerwith[Trigger] = none || h1.flowto_triggerwith[Trigger] in c1.inner.contains.*contains
	
	//History should never be reached from (somewhere, possibly nested) inside their own compound state
	all h1: History, c1: CompositeState | h1 in (c1.s_possess + c1.d_possess + c1.inner.s_possess + c1.inner.d_possess) 
							=> h1.flowfrom_triggerwith[Trigger] not in (c1.^contains + c1.inner.contains.*contains + c1.^contains.inner.contains.*contains - StartState)

	all h1: History, t1: Trigger | h1.flowto_triggerwith[t1] != none => t1.notated = none //Leaving transitions of history must be unconditional
}

// if a composite state contains region states, then all states are contained by region states directly and in the composite state indirectly, so the composite state doesn't contain any states directly, history as well
fact{
	all s1: State | s1 in Region.contains => s1 not in CompositeState.contains
	all h1: History | h1 in (Region.s_possess + Region.d_possess) => h1 not in (CompositeState.s_possess + CompositeState.d_possess)
	all c1: CompositeState, s1: State | #c1.inner > 0 => s1 not in c1.contains
	all c1: CompositeState, h1: History | #c1.inner > 0 => h1 not in (c1.s_possess + c1.d_possess)
}

// rules of "substates"
fact{
	//No compound or region may be empty or contain only history/fork/join nodes.
	State & (NormalState + CompositeState) != none 
	#CompositeState > 0 && #CompositeState.inner = 0 => CompositeState.contains & (NormalState + CompositeState) != none 
	#Region > 0 => Region.contains & (NormalState + CompositeState) != none
}

//About names of states (and of regions etc.)
fact{
	// Entities which are "neighbours" (in the sense of living directly side by side in the same compound or region, but not in two parallel regions of the same compound or such), they must not have the same name.
	all s1, s2: State, c1: CompositeState | s1 != s2 && (s1 in c1.contains && s2 in c1.contains) => s1.named != s2.named
	all s1, s2: State, r1: Region | s1 != s2 && (s1 in r1.contains && s2 in r1.contains) => s1.named != s2.named
	all s1, s2: State | s1 != s2 && (s1 not in (Region.contains + CompositeState.contains)) && (s2 not in (Region.contains + CompositeState.contains))  => s1.named != s2.named
	all r1, r2: Region, c1: CompositeState |  r1 != r2 && r1 in c1.inner && r2 in c1.inner => r1.named != r2.named

	//In a compound or region, the name of the outermost level must not be repeated anywhere deeper inside.
//	all c1: CompositeState |  c1.named != c1.^contains.named && c1.named != c1.inner.named && c1.named != c1.inner.contains.named
//	all r1: Region | r1.named != r1.contains.named
}

//About transition labels
fact{
	one t1: Trigger | #t1.notated  = 0 //No need to have many unconditional triggers to express unconditional transitions
	all s1: State, n1:Node, t1: Trigger | s1 in n1.flowfrom_triggerwith[t1] => #s1.flow_triggerwith[t1] = 0 else #s1.flow_triggerwith[t1] < 2//Transitions leaving one state can't have the same triggers

	//One state shouldn't be left with both a conditional and an unconditional transition
	all s1: State, t1, t2: Trigger | (t1.notated = none && t1 != t2 && s1.flow_triggerwith[t1] != none) => s1.flow_triggerwith[t2] = none && s1 not in Node.flowfrom_triggerwith[t2]
	all s1: State, n1, n2: Node, t1, t2: Trigger | (s1 in n1.flowfrom_triggerwith[t1] && t1.notated = none && t1 != t2 && n1 != n2) => s1.flow_triggerwith[t2] = none && s1 not in n2.flowfrom_triggerwith[t2]
}

//About reachability: all states are reachable
fact{
	all n1: NormalState | n1 in (NormalState.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger])
	all c1: CompositeState | (c1.contains + c1.inner.contains) not in (NormalState.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger]) => c1 in (NormalState.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger])
}

//check
run {} for 5 but exactly 1 ForkNode, exactly 2 Trigger
