// States: a start state, a end state, a normal state or a composite state
abstract sig State{
	to: set State
}


abstract sig StartState extends State{

}

abstract sig EndState extends State{

}
{
	#to = 0
}

// Normal states
abstract sig NormalState extends State{

}

// Region
abstract sig Region{
	contains: disj set State, // a region can contain normal states and composite states
	partof: one CompositeState // a region states can only belong to a composite state
}

//Composite states
abstract sig CompositeState extends State{
	contains: disj set NormalState, // a composite state can contain all states
	belongs: lone CompositeState, // a composite state can only belong to a composite state
	inner: set Region // region states are in composite states
}

fact{
	all s1: StartState, s2: State | s1 not in s2.to // no transitions to start states

}
fact{
	no c1: CompositeState | c1 in c1.^belongs  // this relation has no loop
	partof = ~inner  //reverse relation
	all c1: CompositeState | #c1.inner !=1 && c1 not in c1.to // Region states divide a state diagram into at least two areas that run in parallel
	all s1: State, c1: CompositeState | s1 in c1.contains => c1 not in s1.to // a normal state in a composite state can't have transition to this composite state
	all c1, c2: CompositeState | c1 in c2.^belongs => c1 not in c2.^belongs.to // a composite state in another composite state can't have transition to the composite state
}

// if a composite state contains region states, then all states are contained by region states directly and in the composite state indirectly, so the composite state doesn't contain any states directly
fact{
	all s1: State | s1 in Region.contains => s1 not in CompositeState.contains
	all c1, c2: CompositeState, s1: State | #c1.inner > 0 => s1 not in c1.contains &&  c1 not in c2.^belongs
}

//In a same omposite state, states in different region states can't be transited to each other
fact{
	all r1, r2: Region |  r1.partof = r2.partof => r2.contains not in r1.contains.to
	all r1, r2: Region |  r1.partof = r2.partof => r1.contains not in  r2.contains.to
}

//check
run {} for 4 
