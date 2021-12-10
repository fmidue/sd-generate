module components_sig // All signatures and some direct constraints in this module

// All componets are a node, this is a super class
abstract sig Node{
	flowto_triggerwith: (EmptyTrigger + NonEmptyTrigger) set -> set Node // NoneEmptyLabel is a label of transitions and EmptyLabel represents no label.
}

// Name: EmptyTrigger + NonEmptyName + NonEmptyTrigger
abstract sig Name{}
{
	this in (Node.flowto_triggerwith.Node + State.named + Region.named) // No idenpendent names
}

abstract sig NonEmptyName extends Name{} // It is a name space of states and regions

one abstract sig EmptyTrigger extends Name{} // It represents unconditinonal triggers

abstract sig NonEmptyTrigger extends Name{} // It is a name space of triggers

// States: NormalState + CompositeState + EndState
abstract sig State extends Node{
	named: lone NonEmptyName
}

abstract sig StartState extends Node{}
{
	// Start states are left by one unconditionally transition
	one flowto_triggerwith // One leaving transition
	flowto_triggerwith.(Node - this) = EmptyTrigger // Start states are not left by an arrow with non-empty transition label
	disj [JoinNode, flowto_triggerwith[Name]] // No transitions between start states and join nodes (It excludes the example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample2.svg“)	
}

abstract sig EndState extends State{}
{
	no named // There is no need to mark an end state
	no flowto_triggerwith // No leaving transitions, only coming transitions
}

abstract sig NormalState extends State{
//	has_flowto: set (NormalState - this) // It is used to check true reachability, so reflexive transitions can be excluded
}
	
abstract sig Region{
	named: lone NonEmptyName, // Regions have a name
	r_contains: disj set Node // A region can contain normal states and composite states
}
{
	this in RegionsState.inner // No regions exist independtly
	r_contains in (StartState + EndState) => no inner.this.flowto_triggerwith // If a region has only a start state or a end state, a leaving transition is superfluous for its regions state
}

// Composite states: HierarchicalState + RegionsState 
abstract sig CompositeState extends State{}

// HierarchicalState: Composite states without regions
abstract sig HierarchicalState extends CompositeState{
	h_contains: disj set Node
}
{
	h_contains in (StartState + EndState) => no flowto_triggerwith // If a hierarchical state has only a start state or a end state, a leaving transition is superfluous.
}

// RegionState: Composite states with regions
abstract sig RegionsState extends CompositeState{
	inner: disj some Region // Regions are in region states
}
{
	no named // Region states have no name themselves
	not (one inner) // There can't be only one region in Region states
}

abstract sig ForkNode extends Node{}
{
	// It should be 1 to n(n > 1), n to n is not allowed
	one t1: Name | not (lone flowto_triggerwith[t1]) && no flowto_triggerwith[Name - t1] // It constrains the number of leaving transition > 1 and leaving transitions from fork nodes should all have same conditions or no conditions
}

abstract sig JoinNode extends Node{}
{
	// It should be n(n >= 2) to 1, n to n is not allowed
	one flowto_triggerwith // It constrains the number of leaving transition = 1
	this not in flowto_triggerwith[Name] // No self-loop transition
}

// A specail node History: ShallowHistory + DeepHistory
abstract sig History extends Node{}
{
	// History nodes are left by at most one unconditionally leaving transition
	lone flowto_triggerwith // At most one leaving transition
	one flowto_triggerwith => flowto_triggerwith.(Node - this) = EmptyTrigger // The leaving transition of history shouldn't have conditions
	disj [JoinNode, flowto_triggerwith[Name]] // No transitions between history nodes and fork/join nodes
}

abstract sig ShallowHistory extends History{}

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
