// States: a start state, a end state, a normal state or a composite state
abstract sig State{
	contains: set State,
	flow_triggerwith: set Trigger -> some State 
}

//Node: a fork node or a joint node
abstract sig Node{
	flowfrom_triggerwith: set Trigger -> State,
	flowto_triggerwith: set Trigger -> State
}

// A trigger condition is notated with Char
abstract sig Char{

}
{
	no c1: Char | c1 not in Trigger.notated // no char exists independently
}

// Trigger condition
sig Trigger{
	notated: disj lone Char // if it is noated with no char, it is an unconditional trigger
}

// Normal states
sig NormalState extends State{

}
{
	#contains = 0 // a normal state can't contains other states
}

// A start state is a special normal state
sig StartState extends NormalState{

}

// An end state is a special normal state
sig EndState extends NormalState{

}
{
	#flow_triggerwith = 0
	#this < 2
}

// Region
sig Region{
	contains: set State, // a region can contain normal states and composite states
	partof: one CompositeState // a region states can only belong to a composite state
}

//Composite states
sig CompositeState extends State{
	inner: set Region // region states are in composite states
}
{
	#inner > 1  // Regions divide a composite state into at least two areas that run in parallel
}

sig forkNode extends Node{

}
{
	#flowfrom_triggerwith[Trigger] = 1
	#flowto_triggerwith[Trigger] > 1
}

sig joinNode extends Node{

}
{
	#flowfrom_triggerwith[Trigger] > 1
	#flowto_triggerwith[Trigger] = 1
}

pred noCrossing [r1, r2: Region]{
	 r2.contains not in r1.contains.flow_triggerwith[Trigger] && r1.contains not in  r2.contains.flow_triggerwith[Trigger]
}

//rules of the start state
fact{
	all s1: StartState, s2: State | s1 not in s2.flow_triggerwith[Trigger] // no transitions to start states
}

fact{
	no c1: CompositeState | c1 in c1.^contains // this relation has no loop
	partof = ~inner  //reverse relation
}

//rules of regions
fact{
	all r1, r2: Region |  r1.partof = r2.partof => noCrossing [r1, r2] //In a same omposite state, states in different region states can't be transited to each other
}

// if a composite state contains region states, then all states are contained by region states directly and in the composite state indirectly, so the composite state doesn't contain any states directly
fact{
	all s1: State | s1 in Region.contains => s1 not in CompositeState.contains
	all c1, c2: CompositeState, s1: State | #c1.inner > 0 => s1 not in c1.contains &&  c1 not in c2.^contains
}

//check
run {} for 2 but exactly 2 NormalState
