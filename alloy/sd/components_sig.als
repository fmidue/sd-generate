module components_sig // All signatures and some direct constraints in this module

// All componets are a node, this is a super class
abstract sig Nodes{
	flowto_triggerwith: (EmptyTriggers + NonEmptyTriggers) set // NonEmptyTriggers is a set of labels of transitions and EmptyTriggers represents no label.
				   -> set (Nodes - StartStates) // No coming transitions to start states 
}

// Names: EmptyTriggers + NonEmptyNames + NonEmptyTriggers
abstract sig Names{}
{
	this in (Nodes.flowto_triggerwith.Nodes + States.named + Regions.named) // No idenpendent names
}

abstract sig NonEmptyNames extends Names{} // It is a name space of states and regions

lone abstract sig EmptyTriggers extends Names{} // It represents unconditinonal triggers

abstract sig NonEmptyTriggers extends Names{} // It is a name space of triggers

// States: NormalStates + CompositeStates + EndStates
abstract sig States extends Nodes{
	named: lone NonEmptyNames
}

abstract sig StartStates extends Nodes{}
{
	// Start states are left by one unconditionally transition
	one flowto_triggerwith // One leaving transition
	flowto_triggerwith.Nodes = EmptyTriggers // Start states are not left by an arrow with non-empty transition label
	disj [JoinNodes, flowto_triggerwith[Names]] // No transitions between start states and join nodes (It excludes the example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample2.svg“)	
}

abstract sig EndStates extends States{}
{
	no named // There is no need to mark an end state
	no flowto_triggerwith // No leaving transitions, only coming transitions
}

abstract sig NormalStates extends States{
//	has_flowto: set NormalState - this // It is used to check true reachability, so reflexive transitions can be excluded
}
	
abstract sig Regions{
	named: lone NonEmptyNames, // Regions have a name
	r_contains: disj set Nodes // A region can contain normal states and composite states
}
{
	this in RegionsStates.inner // No regions exist independtly
	r_contains in (StartStates + EndStates) => no inner.this.flowto_triggerwith // If a region has only a start state or a end state, a leaving transition is superfluous for its regions state
}

// Composite states: HierarchicalState + RegionsState 
abstract sig CompositeStates extends States{}

// HierarchicalState: Composite states without regions
abstract sig HierarchicalStates extends CompositeStates{
	h_contains: disj set Nodes
}
{
	h_contains in (StartStates + EndStates) => no flowto_triggerwith // If a hierarchical state has only a start state or a end state, a leaving transition is superfluous.
}

// RegionState: Composite states with regions
abstract sig RegionsStates extends CompositeStates{
	inner: disj some Regions // Regions are in region states
}
{
	no named // Region states have no name themselves
	not (one inner) // There can't be only one region in Region states
}

abstract sig ForkNodes extends Nodes{}
{
	// It should be 1 to n(n > 1), n to n is not allowed
	one t1: Names | not (lone flowto_triggerwith[t1]) && no flowto_triggerwith[Names - t1] // It constrains the number of leaving transition > 1 and leaving transitions from fork nodes should all have same conditions or no conditions
	EndStates not in flowto_triggerwith[Names] // No transitions between end states and fork nodes (see example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample3.svg") 
}

abstract sig JoinNodes extends Nodes{}
{
	// It should be n(n >= 2) to 1, n to n is not allowed
	one flowto_triggerwith // It constrains the number of leaving transition = 1
	this not in flowto_triggerwith[Names] // No self-loop transition
}

// A specail node HistoryNodes: ShallowHistoryNodes + DeepHistoryNodes
abstract sig HistoryNodes extends Nodes{}
{
	// History nodes are left by at most one unconditionally leaving transition
	lone flowto_triggerwith // At most one leaving transition
	one flowto_triggerwith => flowto_triggerwith.(Nodes - this) = EmptyTriggers // The leaving transition of history shouldn't have conditions
	disj [JoinNodes, flowto_triggerwith[Names]] // No transitions between history nodes and join nodes
}

abstract sig ShallowHistoryNodes extends HistoryNodes{}

abstract sig DeepHistoryNodes extends HistoryNodes{}

// It gets all nodes in same and deeper levels of composite states
fun getAllNodesInSameAndDeeperLevel [h1: HierarchicalStates]: Nodes{
	h1.^h_contains.*(inner.r_contains.(iden + ^h_contains))
}

// It gets all regions in same and deeper levels of composite states
fun getAllRegionsInSameAndDeeperLevel [r1: RegionsStates]: Regions{
	r1.inner.(iden + ^(r_contains.(iden + ^h_contains).inner))
}

// It gets all nodes in same and deeper levels of regions
fun getAllNodesInSameAndDeeperLevel [r1: Regions]: Nodes{
	r1.r_contains.(iden + ^h_contains).*(inner.r_contains.(iden + ^h_contains))
}

// It gets all regions in same and deeper levels of regions
fun getAllRegionsInSameAndDeeperLevel [r1: Regions]: Regions{
	r1.r_contains.(iden + ^h_contains).inner.(iden + ^(r_contains.(iden + ^h_contains).inner))
}
