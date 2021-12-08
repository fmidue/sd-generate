module components_sig // All signatures and some direct constraints in this module

// All componets are a node, this is a super class
abstract sig Node{
	flowto_triggerwith: Trigger set -> set Node // Int is a label of transitions and 0 represents no label.
}

// Trigger condition
abstract sig Trigger{
	notated: disj lone Int // A kind of trigger maps a char, if it is noated with no char, it is an unconditional trigger
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
	
// Region
abstract sig Region{
	named: lone Int, // Regions have a name
	r_contains: disj set Node // A region can contain normal states and composite states
}
{
	this in RegionsState.inner
}

// Composite states: Hierarchical states and region states 
abstract sig CompositeState extends State{}

// HierarchicalState: Composite states without regions
abstract sig HierarchicalState extends CompositeState{
	h_contains: disj set Node,
}

// RegionState: Composite states with regions
abstract sig RegionsState extends CompositeState{
	inner: disj set Region // Region states are in composite states
}
{
	no named
}

//A special node: fork nodes
abstract sig ForkNode extends Node{}
{
	// It should be 1 to n(n > 1), n to n is not allowed
	one t1: Trigger | not (lone flowto_triggerwith[t1]) // It constrains the number of leaving transition > 1 and for fork nodes, leaving transitions should all have same conditions or no conditions
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
fun getAllNodeInSameAndDeeperLevel [h1: HierarchicalState]: Node{
	h1.^h_contains.*(inner.r_contains.(iden + ^h_contains))
}

// It gets all regions in same and deeper levels of composite states
fun getAllRegionInSameAndDeeperLevel [r1: RegionsState]: Region{
	r1.inner.(iden + ^(r_contains.(iden + ^h_contains).inner))
}

// It gets all nodes in same and deeper levels of regions
fun getAllNodeInSameAndDeeperLevel [r1: Region]: Node{
	r1.r_contains.(iden + ^h_contains).*(inner.r_contains.(iden + ^h_contains))
}

// It gets all regions in same and deeper levels of regions
fun getAllRegionInSameAndDeeperLevel [r1: Region]: Region{
	r1.r_contains.(iden + ^h_contains).inner.(iden + ^(r_contains.(iden + ^h_contains).inner))
}
