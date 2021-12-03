module components_sig // All signatures and some direct constraints in this module

//All componets are a node, this is a super class
abstract sig Node{
	flowto_triggerwith: Trigger set -> set Node
}

// Trigger condition
abstract sig Trigger{
	notated: disj lone Int // A kind of trigger maps a char, if it is noated with no char, it is an unconditional trigger
}
{
	// names of trigger are different from states and regions
	notated & State.named = none
	notated & Region.named = none
}

// States: a normal state or a composite state
abstract sig State extends Node{
	named: lone Int
}

// Start state: a node
abstract sig StartState extends Node{}
{
	some flowto_triggerwith //At least one leaving transition
}

// An end state is a special state
abstract sig EndState extends State{}
{
	no named // There is no need to mark an end state
	no flowto_triggerwith // no leaving transitions, only coming transitions
}

// Normal states
abstract sig NormalState extends State{}
{
	one named // a normal state must have a name
}
	
// Region
abstract sig Region{
	named: one Int, // regions have a name
	contains: disj set Node, // a region can contain normal states and composite states
	partof: one CompositeState // a region can only belong to a composite state
}

//Composite states
abstract sig CompositeState extends State{
	contains: disj set Node,
	inner: disj set Region // region states are in composite states
}
{
	#inner != 1 // Regions divide a composite state into at least two areas that run in parallel
	#inner = 0 => #named = 1 // If a composite dosn't have regions, it must have a name
	#inner > 1 => #named = 0 // If a composite have regions, it dosen't need a name
}

//A special node: fork nodes
abstract sig ForkNode extends Node{}
{
	//It should be 1 to n(n > 1), n to n is not allowed
	one t1:Trigger | #flowto_triggerwith[t1] > 1 // It constrains the number of leaving transition > 1
}

// A special node: join nodes
abstract sig JoinNode extends Node{}
{
	//It should be n(n >= 2) to 1, n to n is not allowed
	one flowto_triggerwith
}

// A specail node: History: Shallow History and Deep History
abstract sig History extends Node{}
{
	all t1: Trigger | flowto_triggerwith[t1] != none => t1.notated = none // The leaving transition of history shouldn't have conditions
}

// A special node: shallow history
abstract sig ShallowHistory extends History{}

// A special node: deep history
abstract sig DeepHistory extends History{}

// It gets all nodes in same and deeper levels of composite
fun getAllSameAndDeeperLevel [c1: CompositeState]: State{
	c1.^contains.*(inner.contains.(iden + ^contains))
}
