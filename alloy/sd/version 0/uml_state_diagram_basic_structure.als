// All componets are a node, this is a super class
abstract sig Nodes{}

// States: NormalStates + CompositeStates
abstract sig States extends Nodes{}

abstract sig NormalStates extends States{}

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
        not (one contains) // There can't be only one region in Region states
}

abstract sig Regions{
        contains: disj some Nodes
}
{
        this in RegionsStates.contains // No regions exist independtly
}

// It gets all nodes in same and deeper levels of a composite state
fun nodesInThisAndDeeper[c1: CompositeStates] : set Nodes {
        c1.^(HierarchicalStates <: contains).*((RegionsStates <: contains).(Regions <: contains).*(HierarchicalStates <: contains))
        + c1.^((RegionsStates <: contains).(Regions <: contains).*(HierarchicalStates <: contains))
}

// A composite state can't appear in in deeper level of itself
pred acyclicContain{
        all c1: CompositeStates | c1 not in nodesInThisAndDeeper[c1] // A composite state can't appear in deeper level of itself
}

// Other rules
fact{
        acyclicContain
        disj[Regions.contains, HierarchicalStates.contains] // No same nodes are contained by different objects
}

run {} for 3 but exactly 1 RegionsStates, exactly 1 HierarchicalStates, exactly 2 Regions, exactly 2 NormalStates
