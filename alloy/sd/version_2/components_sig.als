module components_sig // All signatures and some direct constraints in this module

// Flows is corresponding to actual flows
sig Flows{
        from: one (Nodes - EndNodes), // End nodes have no leaving transitions
        to: one (Nodes - StartNodes), // Start nodes have no coming transitions
        label: one Triggers
}

// All componets are a node, this is a super class
abstract sig Nodes{}

// The name space of all components
abstract sig ComponentNames{}{
        this in (States.name + Regions.name)
}

// The name space of all Triggers
abstract sig Triggers{}

one abstract sig EmptyTrigger extends Triggers{}

abstract sig TriggerNames extends Triggers{}{
        this in Flows.label
}

// States: NormalStates + CompositeStates
abstract sig States extends Nodes{
        name: lone ComponentNames
}

abstract sig StartNodes extends Nodes{}
{
        // Start nodes are left by one unconditionally transition
        one from.this // Start nodes have only one leaving transition
        from.this.label = EmptyTrigger // Start states are not left by an arrow with non-empty transition label
}

abstract sig EndNodes extends Nodes{}

abstract sig NormalStates extends States{}

abstract sig Regions{
        name: lone ComponentNames, // Regions have at most one name
        contains: disj some Nodes
}
{
        this in RegionsStates.contains // No regions exist independtly
}

// Composite states: HierarchicalState + RegionsState
abstract sig CompositeStates extends States{}

// HierarchicalState: Composite states without regions
abstract sig HierarchicalStates extends CompositeStates{
        contains: disj some Nodes
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
        not (lone from.this)  // Each fork node has two or more leaving arrows
        one to.this // It constrains the number of coming transition = 1
        one from.this.label // For fork nodes, leaving transitions should all have same conditions
}

abstract sig JoinNodes extends Nodes{}
{
        // It should be n(n >= 2) to 1, n to n is not allowed
        one from.this // It constrains the number of leaving transition = 1
        not (lone to.this) // Each join node has two or more entering arrows
        one to.this.label // For join nodes, comming transitions should all have same conditions
}

// HistoryNodes: ShallowHistoryNodes + DeepHistoryNodes
abstract sig HistoryNodes extends Nodes{}
{
        // History nodes are left by at most one unconditionally leaving transition
        lone from.this // At most one leaving transition
        from.this.label in EmptyTrigger // The leaving transition of history shouldn't have conditions
        this not in from.this.to // No self-loop transition
}

abstract sig ShallowHistoryNodes extends HistoryNodes{}

abstract sig DeepHistoryNodes extends HistoryNodes{}

// It gets all nodes contained by a composite state.
fun nodesInThis[c1: CompositeStates] : set Nodes {
        c1.(HierarchicalStates <: contains) + c1.(RegionsStates <: contains).(Regions <: contains)
}

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

// It gets all contained nodes, namely nodes inside composite states
fun allContainedNodes[]: set Nodes{HierarchicalStates.contains + Regions.contains}

// It gets all nodes contained by other parallel regions
fun nodesInOtherParallelRegions[rs: set Regions]: set Nodes{
        ((contains.rs).(RegionsStates <: contains) - rs).contains
}
