// States: a start state, a end state, a normal state or a composite state
abstract sig State{
	contains: set State,
	flow_triggerwith: Trigger set -> set State 
}

//Node: a fork node or a joint node
abstract sig Node{
	flowfrom_triggerwith: Trigger set -> set State,
	flowto_triggerwith: Trigger set -> set State
}

// A trigger condition is notated with Char
abstract sig Char{

}
{
	no c1: Char | c1 not in Trigger.notated // no char exists independently
}

// Trigger condition
sig Trigger{
	notated: disj lone Char // A kind of trigger maps a char, if it is noated with no char, it is an unconditional trigger
}

// Normal states
sig NormalState extends State{

}
{
	#contains = 0 // a normal state can't contains other states
}

// A start state is a special normal state
sig StartState extends State{

}
{
	#contains = 0 // a start state can't contains other states
	#flow_triggerwith > 0 //At least one leaving transitions
}

// An end state is a special normal state
sig EndState extends State{

}
{
	#contains = 0 // an end state can't contains other states
	#flow_triggerwith = 0 // no leaving transitions(to states), only coming transitions
}
	
// Region
sig Region{
	contains: disj set State, // a region can contain normal states and composite states
	partof: one CompositeState // a region states can only belong to a composite state
}

//Composite states
sig CompositeState extends State{
	inner: disj set Region // region states are in composite states
}
{
	#inner > 1  // Regions divide a composite state into at least two areas that run in parallel
}

//A special node: fork nodes
sig ForkNode extends Node{

}
{
	//It should be 1 to n(n >= 2), n to n is not allowed
	#flowfrom_triggerwith[Trigger] = 1
	#flowfrom_triggerwith = 1
	one t1:Trigger | #flowto_triggerwith[t1] > 0
	#flowto_triggerwith > 1
}

// A special node: join nodes
sig JoinNode extends Node{

}
{
	//It should be n(n >= 2) to 1, n to n is not allowed
	one t1:Trigger | #flowfrom_triggerwith[t1] > 0
	#flowfrom_triggerwith > 1
	#flowto_triggerwith[Trigger] = 1
	#flowto_triggerwith = 1
}

/*sig ShallowHistory extends Node{

}

sig DeepHistory extends Node{

}*/

pred noCrossing [r1, r2: Region]{
	 r2.contains not in r1.contains.flow_triggerwith[Trigger] && r1.contains not in  r2.contains.flow_triggerwith[Trigger]
}


//rules of start states and end states
fact{
	#StartState > 0
	#EndState < 2
	all s1: StartState| s1 not in State.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger] // no transitions to start states
	all r1: Region, s1: StartState | s1 in r1.contains => #s1 < 2 //In regions, there is at most one start state.
	all c1: CompositeState, s1: StartState | s1 in c1.contains => #s1 < 2 //In composite states, there is at most one start state.
	#CompositeState = 0 => #StartState < 2 //if no regions and composites, there is also at most one start state.
	all e1: EndState | e1 in (State.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger]) &&  e1 not in Node.flowfrom_triggerwith[Trigger]//no leaving transitions(including to nodes), only coming transitions, but if an end state exists, there must be transitions to it
}
//rules of composite states
fact{
	no c1: CompositeState | c1 in c1.^contains // this relation has no loop
	partof = ~inner  //reverse relation
	all c1: CompositeState | c1.inner = none => c1.contains & (NormalState + CompositeState) != none // only start state, end start and nodes in composite states is unallowed
}

//rules of regions
fact{
	all r1, r2: Region |  r1.partof = r2.partof => noCrossing [r1, r2] //In a same omposite state, states in different region states can't be transited to each other
	all r1: Region, c1: CompositeState | r1 in c1.inner => c1 not in r1.contains // if a region is in a composite state, it can't contain the composite state, otherwise it has a loop relation
	all f1: ForkNode, c1: CompositeState | f1.flowto_triggerwith[Trigger] & c1.inner.contains != none  => f1.flowto_triggerwith[Trigger] in c1.inner.contains && #f1.flowto_triggerwith[Trigger] = #c1.inner // fork node lead to all regions paralell to each other
	all  j1: JoinNode, c1: CompositeState | j1.flowfrom_triggerwith[Trigger] & c1.inner.contains != none => j1.flowfrom_triggerwith[Trigger] in c1.inner.contains && #j1.flowfrom_triggerwith[Trigger] = #c1.inner // join nodes merges flows of all regions paralell to each other.
	no r1: Region | r1.contains & (NormalState + CompositeState) = none // only start state, end start and nodes in regions is unallowed
}

//rules of fork nodes and join nodes
fact{
	all f1: ForkNode, t1, t2: Trigger | f1.flowto_triggerwith[t1] != none && f1.flowto_triggerwith[t2] != none => t1=t2 // for fork nodes, leaving transitions should have same conditions or no conditions
	all j1: JoinNode, t1, t2: Trigger | j1.flowfrom_triggerwith[t1] != none && j1.flowfrom_triggerwith[t2] != none => t1=t2 // for join nodes, comming transitions should have same conditions or no conditions
	all f1: ForkNode, t1, t2:Trigger | f1.flowfrom_triggerwith[t1] & (f1.flowto_triggerwith[t1] + f1.flowto_triggerwith[t2]) = none // no loop arcs for fork nodes
	all j1: JoinNode, t1, t2:Trigger | j1.flowfrom_triggerwith[t1] not in Region.contains => (j1.flowto_triggerwith[t1] + j1.flowto_triggerwith[t2]) & (j1.flowfrom_triggerwith[t1] + j1.flowfrom_triggerwith[t2]) = none  // if states are not in regions, no loop arcs for join nodes
}

// if a composite state contains region states, then all states are contained by region states directly and in the composite state indirectly, so the composite state doesn't contain any states directly
fact{
	all s1: State | s1 in Region.contains => s1 not in CompositeState.contains
	all c1, c2: CompositeState, s1: State | #c1.inner > 0 => s1 not in c1.contains &&  c1 not in c2.^contains
}

// other rules
fact {
	one t1: Trigger | #t1.notated  = 0 //No need to have many unconditional triggers to express unconditional transitions
	all s1: State, t1: Trigger | #s1.flow_triggerwith[t1] < 2 //Transitions leaving one state can't have the same triggers
	all s1: State, t1, t2: Trigger | s1.flow_triggerwith[t1] != none && t1.notated = none => s1.flow_triggerwith[t1] & s1.flow_triggerwith[t2] = none //One state shouldn't be left with both a conditional and an unconditional transition
	State & (NormalState + CompositeState) != none
 }
//check
run {} for 4 but  exactly 2 Trigger
