module uml_state_diagram

open components_sig as components

pred noCrossing [r1, r2: Region]{
	 r2.contains not in r1.contains.flow_triggerwith[Trigger] && r1.contains not in  r2.contains.flow_triggerwith[Trigger]
}


//rules of start states and end states
fact{
	#StartState > 0 // at least one start state
	#EndState < 2 // at most one end state
	all s1: StartState, t1: Trigger | s1.flow_triggerwith[t1] != none => t1.notated = none
	all s1: StartState| s1 not in State.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger] // no transitions to start states
			    && s1 not in JoinNode.flowfrom_triggerwith[Trigger] // no transitions between start states and fork nodes
	all r1: Region, s1: StartState | s1 in r1.contains => #s1 < 2 //In regions, there is at most one start state.
	all c1: CompositeState, s1: StartState | s1 in c1.contains => #s1 < 2 //In composite states, there is at most one start state.
	#CompositeState = 0 => #StartState < 2 && #DeepHistory < 2 && #ShallowHistory < 2 //if no regions and composites, there is also at most one start state, one deep history and one shallow history.
	all e1: EndState | e1 in (State.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger]) &&  e1 not in Node.flowfrom_triggerwith[Trigger]//no leaving transitions(including to nodes), only coming transitions, but if an end state exists, there must be transitions to it
			    && e1 not in ForkNode.flowto_triggerwith[Trigger] // no transitions between end states and join nodes
}
//rules of composite states
fact{
	no c1: CompositeState | c1 in c1.^contains // this relation has no loop
	partof = ~inner  //reverse relation
	all c1: CompositeState | c1.inner = none => c1.contains & (NormalState + CompositeState) != none // only start state, end start and nodes in composite states is unallowed
}

//rules of regions
fact{
	all r1, r2: Region |  r1.partof = r2.partof => noCrossing [r1, r2] //In a same composite state, states in different region states can't be transited to each other
	all r1: Region, c1: CompositeState | r1 in c1.inner => c1 not in r1.contains // if a region is in a composite state, it can't contain the composite state, otherwise it has a loop relation
	all f1: ForkNode, c1: CompositeState | f1.flowto_triggerwith[Trigger] & c1.inner.contains != none  => f1.flowto_triggerwith[Trigger] in c1.inner.contains && #f1.flowto_triggerwith[Trigger] = #c1.inner // fork node lead to all regions paralell to each other
	all  j1: JoinNode, c1: CompositeState | j1.flowfrom_triggerwith[Trigger] & c1.inner.contains != none => j1.flowfrom_triggerwith[Trigger] in c1.inner.contains && #j1.flowfrom_triggerwith[Trigger] = #c1.inner // join nodes merges flows of all regions paralell to each other.
	no r1: Region | r1.contains & (NormalState + CompositeState) = none // only start state, end start and nodes in regions is unallowed
}

//rules of fork nodes and join nodes
fact{
	all f1: ForkNode, t1, t2: Trigger | f1.flowto_triggerwith[t1] != none && f1.flowto_triggerwith[t2] != none => t1 = t2 // for fork nodes, leaving transitions should have same conditions or no conditions
	all j1: JoinNode, t1, t2: Trigger | j1.flowfrom_triggerwith[t1] != none && j1.flowfrom_triggerwith[t2] != none => t1 = t2 // for join nodes, comming transitions should have same conditions or no conditions
	all f1: ForkNode, t1, t2:Trigger | f1.flowfrom_triggerwith[t1] & (f1.flowto_triggerwith[t1] + f1.flowto_triggerwith[t2]) = none // no loop arcs for fork nodes
	all j1: JoinNode, t1, t2:Trigger | j1.flowfrom_triggerwith[t1] not in Region.contains => (j1.flowto_triggerwith[t1] + j1.flowto_triggerwith[t2]) & (j1.flowfrom_triggerwith[t1] + j1.flowfrom_triggerwith[t2]) = none  // if states are not in regions, no loop arcs for join nodes
}

//rules of History
fact{
	// a shallow history should be directed to a same or the first deeper level, but definitely not to a level further outside
	all h1: ShallowHistory, c1: CompositeState |  #c1.inner = 0 && h1 in c1.s_possess => h1.flowto_triggerwith[Trigger] = none || h1.flowto_triggerwith[Trigger] in (c1.contains + c1.contains.contains) 
	all h1: ShallowHistory, c1: CompositeState |  #c1.inner > 0 && h1 in c1.s_possess => h1.flowto_triggerwith[Trigger] = none || h1.flowto_triggerwith[Trigger] in (c1.inner.contains + c1.inner.contains.contains)

	// a deep history should be directed to a same or a deeper level, but definitely not to a level further outside
	all h1: DeepHistory, c1: CompositeState |  #c1.inner = 0 && h1 in c1.d_possess => h1.flowto_triggerwith[Trigger] = none || h1.flowto_triggerwith[Trigger] in c1.^contains
	all h1: DeepHistory, c1: CompositeState |  #c1.inner > 0 && h1 in c1.inner.d_possess => h1.flowto_triggerwith[Trigger] = none || h1.flowto_triggerwith[Trigger] in c1.inner.contains.*contains

	all h1: History, c1: CompositeState | h1 in (c1.s_possess + c1.d_possess + c1.inner.s_possess + c1.inner.d_possess) => h1.flowfrom_triggerwith[Trigger] not in (c1.^contains + c1.inner.contains.*contains - StartState) //History should never be reached from (somewhere, possibly nested) inside their own compound state
	all h1: History, t1: Trigger | h1.flowto_triggerwith[t1] != none => t1.notated = none //leaving transition of history must be unconditional
}

// if a composite state contains region states, then all states are contained by region states directly and in the composite state indirectly, so the composite state doesn't contain any states directly, history as well
fact{
	all s1: State | s1 in Region.contains => s1 not in CompositeState.contains
	all h1: History | h1 in (Region.s_possess + Region.d_possess) => h1 not in (CompositeState.s_possess + CompositeState.d_possess)
	all c1: CompositeState, s1: State | #c1.inner > 0 => s1 not in c1.contains
	all c1: CompositeState, h1: History | #c1.inner > 0 => h1 not in (c1.s_possess + c1.d_possess)
}

// other rules
fact {
	one t1: Trigger | #t1.notated  = 0 //No need to have many unconditional triggers to express unconditional transitions
	all s1: State, n1:Node, t1: Trigger | s1 in n1.flowfrom_triggerwith[t1] => #s1.flow_triggerwith[t1] = 0 else #s1.flow_triggerwith[t1] < 2//Transitions leaving one state can't have the same triggers

	//One state shouldn't be left with both a conditional and an unconditional transition
	all s1: State, t1, t2: Trigger | (t1.notated = none && t1 != t2 && s1.flow_triggerwith[t1] != none) => s1.flow_triggerwith[t2] = none && s1 not in Node.flowfrom_triggerwith[t2]
	all s1: State, n1, n2: Node, t1, t2: Trigger | (s1 in n1.flowfrom_triggerwith[t1] && t1.notated = none && t1 != t2 && n1 != n2) => s1.flow_triggerwith[t2] = none && s1 not in n2.flowfrom_triggerwith[t2]

	State & (NormalState + CompositeState) != none //We don't want to allow anything that has only "special nodes" like history or fork/joint, but no normal states.
	all n1, n2: Node, t1, t2: Trigger | n1 != n2 => n1.flowfrom_triggerwith[t1] != n2.flowfrom_triggerwith[t1] && n1.flowto_triggerwith[t2] != n2.flowto_triggerwith[t2] //no duplicate nodes

	// Reachability: all states are reachable
	all n1: NormalState | n1 in (NormalState.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger])
	all c1: CompositeState | (c1.contains + c1.inner.contains) not in (NormalState.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger]) => c1 in (NormalState.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger])
 }

//check
run {} for 4
