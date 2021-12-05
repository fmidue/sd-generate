module components_sig // All signatures and some direct constraints in this module

// All componets are a node, this is a super class
abstract sig Node{
	flowto_triggerwith: Trigger set -> set Node
}

// Trigger condition
abstract sig Trigger{
	notated: disj lone Int // A kind of trigger maps a char, if it is noated with no char, it is an unconditional trigger
}
{
	// Names of trigger are different from states and regions
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
	// Start states are left by one unconditionally transition
	one flowto_triggerwith // One leaving transition
	all t1: Trigger | flowto_triggerwith[t1] != none => t1.notated = none // Start states are not left by an arrow with non-empty transition label
	JoinNode & flowto_triggerwith[Trigger] = none// No transitions between start states and join nodes (It excludes the example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample2.svg“)	
}

// An end state is a special state
abstract sig EndState extends State{}
{
	no named // There is no need to mark an end state
	no flowto_triggerwith // No leaving transitions, only coming transitions
}

// Normal states
abstract sig NormalState extends State{}
{
	one named // A normal state must have a name
}
	
// Region
abstract sig Region{
	named: one Int, // Regions have a name
	contains: disj set Node, // A region can contain normal states and composite states
	partof: one CompositeState // A region can only belong to a composite state
}

// Composite states
abstract sig CompositeState extends State{
	contains: disj set Node,
	inner: disj set Region // Region states are in composite states
}
{
	#inner != 1 // Regions divide a composite state into at least two areas that run in parallel
	#inner = 0 => #named = 1 // If a composite dosn't have regions, it must have a name
	#inner > 1 => #named = 0 // If a composite have regions, it dosen't need a name
}

//A special node: fork nodes
abstract sig ForkNode extends Node{}
{
	// It should be 1 to n(n > 1), n to n is not allowed
	one t1:Trigger | #flowto_triggerwith[t1] > 1 // It constrains the number of leaving transition > 1 and for fork nodes, leaving transitions should all have same conditions or no conditions
}

// A special node: join nodes
abstract sig JoinNode extends Node{}
{
	// It should be n(n >= 2) to 1, n to n is not allowed
	one flowto_triggerwith // It constrains the number of leaving transition = 1
	this not in flowto_triggerwith[Trigger] // No self-loop transition
}

// A specail node: History: Shallow History and Deep History
abstract sig History extends Node{}
{
	// History nodes are left by one unconditionally transition
	one flowto_triggerwith // One leaving transition
	all t1: Trigger | flowto_triggerwith[t1] != none => t1.notated = none // The leaving transition of history shouldn't have conditions
	(JoinNode + ForkNode)	& flowto_triggerwith[Trigger] = none // No transitions between history nodes and fork/join nodes
}

// A special node: shallow history
abstract sig ShallowHistory extends History{}

// A special node: deep history
abstract sig DeepHistory extends History{}

// It gets all nodes in same and deeper levels of composite states
fun getAllNodeInSameAndDeeperLevel [c1: CompositeState]: Node{
	c1.^contains.*(inner.contains.(iden + ^contains))
}

// It gets all regions in same and deeper levels of composite states
fun getAllRegionInSameAndDeeperLevel [c1: CompositeState]: Region{
	c1.inner.(iden + ^(contains.(iden + ^contains).inner))
}

// It gets all nodes in same and deeper levels of regions
fun getAllNodeInSameAndDeeperLevel [r1: Region]: Node{
	r1.contains.(iden + ^contains).*(inner.contains.(iden + ^contains))
}

// It gets all regions in same and deeper levels of regions
fun getAllRegionInSameAndDeeperLevel [r1: Region]: Region{
	r1.contains.(iden + ^contains).inner.(iden + ^(contains.(iden + ^contains).inner))
}
