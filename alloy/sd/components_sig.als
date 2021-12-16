module components_sig // All signatures and some direct constraints in this module

// All componets are a node, this is a super class
abstract sig Nodes{
	flow: Triggers set -> set (Nodes - StartNodes), // No coming transitions to start states
}

// Names: EmptyTriggers + ComponentNames + TriggerNames
abstract sig ComponentNames{}{
	this in (States.name + Regions.name)
}

abstract sig Triggers{}

one abstract sig EmptyTrigger extends Triggers{}

abstract sig TriggerNames extends Triggers{}{
	this in (Nodes.flow.Nodes)
}

// States: NormalStates + CompositeStates
abstract sig States extends Nodes{
	name: lone ComponentNames
}

abstract sig StartNodes extends Nodes{
	flag: Int // It is used to check if this can be neglected
}
{
	flag = 0 or flag = 1 // If flag = 0, it can't be neglected, or it can be neglected
	// Start states are left by one unconditionally transition
	one flow // One leaving transition
	flow.Nodes = EmptyTrigger // Start states are not left by an arrow with non-empty transition label
	disj [JoinNodes, flow[EmptyTrigger]] // No transitions between start states and join nodes (It excludes the example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample2.svg“)	
}

abstract sig EndNodes extends Nodes{}
{
	no flow // No leaving transitions, only coming transitions
}

abstract sig NormalStates extends States{}
	
abstract sig Regions{
	name: lone ComponentNames, // Regions have a name
	contains: disj some Nodes // A region can contain normal states and composite states
}
{
	this in RegionsStates.contains // No regions exist independtly
	contains in (StartNodes + EndNodes) implies no (RegionsStates <: contains).this.flow // If a region has only a start state or a end state, a leaving transition is superfluous for its regions state
}

// Composite states: HierarchicalState + RegionsState 
abstract sig CompositeStates extends States{}

// HierarchicalState: Composite states without regions
abstract sig HierarchicalStates extends CompositeStates{
	contains: disj some Nodes
}
{
	contains in (StartNodes + EndNodes) implies no flow // If a hierarchical state has only a start state or a end state, a leaving transition is superfluous.
}

// RegionState: Composite states with regions
abstract sig RegionsStates extends CompositeStates{
	contains: disj some Regions // Regions are in region states
}
{
	no name // Region states have no name themselves
	not (one contains) // There can't be only one region in Region states
}

abstract sig ForkNodes extends Nodes{}
{
	// It should be 1 to n(n > 1), n to n is not allowed
	one t1: Triggers | not (lone flow[t1]) and no flow[Triggers - t1] // It constrains the number of leaving transition > 1 and leaving transitions from fork nodes should all have same conditions or no conditions
	disj [EndNodes, flow[Triggers]] // No transitions between end states and fork nodes (see example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample3.svg") 
}

abstract sig JoinNodes extends Nodes{}
{
	// It should be n(n >= 2) to 1, n to n is not allowed
	one flow // It constrains the number of leaving transition = 1
	this not in flow[Triggers] // No self-loop transition
}

// A specail node HistoryNodes: ShallowHistoryNodes + DeepHistoryNodes
abstract sig HistoryNodes extends Nodes{}
{
	// History nodes are left by at most one unconditionally leaving transition
	lone flow // At most one leaving transition
	no flow[TriggerNames] // The leaving transition of history shouldn't have conditions
	disj [(JoinNodes + this), flow[EmptyTrigger]] // No self-loop transition and transitions between history nodes and join nodes
}

abstract sig ShallowHistoryNodes extends HistoryNodes{}

abstract sig DeepHistoryNodes extends HistoryNodes{}

// It gets all nodes in same and deeper levels of a composite state
fun nodesInThisAndDeeper[c1: CompositeStates] : set Nodes {
    c1.^(HierarchicalStates <: contains).*((RegionsStates <: contains).(Regions <: contains).*(HierarchicalStates <: contains))
 + c1.^((RegionsStates <: contains).(Regions <: contains).*(HierarchicalStates <: contains))
}

// It gets all nodes in same and deeper levels of a region
fun nodesInThisAndDeeper[r1: Regions] : set Nodes {
	r1.contains.*(HierarchicalStates <: contains).*((RegionsStates <: contains).(Regions <: contains).*(HierarchicalStates <: contains))
}

// It gets all regions in same and deeper levels of a composite state
fun regionsInThisAndDeeper[h1: HierarchicalStates]: set Regions{
	h1.^(HierarchicalStates <: contains).(RegionsStates <: contains).*((Regions <: contains).*(HierarchicalStates <: contains).(RegionsStates <: contains)) 
}

// It gets all regions in same and deeper levels of a region
fun regionsInThisAndDeeper[r1: Regions]: set Regions{
	r1.^((Regions <: contains).*(HierarchicalStates <: contains).(RegionsStates <: contains))
}

// Get a set of all nodes inside composite states
fun allContainedNodes[]: set Nodes{HierarchicalStates.contains + Regions.contains}
