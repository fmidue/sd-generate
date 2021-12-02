module components_sig // All signatures and some direct constraints in this module

// States: a start state, a end state, a normal state or a composite state
abstract sig State{
	named: lone Char,
	contains: disj set State,
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
	no c1: Char | c1 not in (Trigger.notated + State.named + Region.named) // no char exists independently
}

// Trigger condition
sig Trigger{
	notated: disj lone Char // A kind of trigger maps a char, if it is noated with no char, it is an unconditional trigger
}
{
	// names of trigger are different from states and regions
	notated & State.named = none
	notated & Region.named = none
}

// Normal states
sig NormalState extends State{

}
{
	#contains = 0 // a normal state can't contains other states
	#named = 1 // a normal state must have a name
}

// A start state is a special normal state
sig StartState extends State{

}
{
	#contains = 0 // a start state can't contains other states
	#flow_triggerwith > 0 //At least one leaving transitions
	#named = 0 // start states don't need a name
}

// An end state is a special normal state
sig EndState extends State{

}
{
	#contains = 0 // an end state can't contains other states
	#flow_triggerwith = 0 // no leaving transitions(to states), only coming transitions
	#named = 0 // end states don't need a name
}
	
// Region
sig Region{
	named: one Char, // regions have a name
	contains: disj set State, // a region can contain normal states and composite states
	partof: one CompositeState, // a region can only belong to a composite state
	s_possess: disj lone ShallowHistory, // a region can possess at most one shallow history (no regions)
	d_possess: disj lone DeepHistory // a region can possess at most one deep history (no regions)
}
{
	named & State.named = none
}

//Composite states
sig CompositeState extends State{
	inner: disj set Region, // region states are in composite states
	s_possess: disj lone ShallowHistory, // a composite state can possess at most one shallow history
	d_possess: disj lone DeepHistory // a composite can possess at most one deep history
}
{
	#inner != 1 // Regions divide a composite state into at least two areas that run in parallel
	#inner = 0 => #named = 1 // If a composite dosn't have regions, it must have a name
	#inner > 1 => #named = 0 // If a composite have regions, it dosen't need a name
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

// History: Shallow History and Deep History
abstract sig History extends Node{
	
}
{
	all t1: Trigger | flowto_triggerwith[t1] != none => t1.notated = none // The leaving transition of history shouldn't have conditions
	#flowfrom_triggerwith > 0 // There should be at least one coming transiton to history.
	#flowto_triggerwith < 2 // There should be at most one leaving transiton from history.
}

// A special node: shallow history
sig ShallowHistory extends History{

}

// A special node: deep history
sig DeepHistory extends History{

}
