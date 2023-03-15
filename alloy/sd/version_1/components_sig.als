module components_sig

// All components are a node, this is a super class
abstract sig Nodes{}

// The name space of all components
abstract sig ComponentNames{}{
        this in (States.name + Regions.name)
}

// States: NormalStates + CompositeStates
abstract sig States extends Nodes{
        name: lone ComponentNames
}

abstract sig StartNodes extends Nodes{}

abstract sig EndNodes extends Nodes{}

abstract sig NormalStates extends States{}

abstract sig Regions{
        name: lone ComponentNames, // Regions have at most one name
        contains: disj some Nodes
}
{
        this in RegionsStates.contains // No regions exist independently
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

abstract sig JoinNodes extends Nodes{}

// HistoryNodes: ShallowHistoryNodes + DeepHistoryNodes
abstract sig HistoryNodes extends Nodes{}

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

// It gets all contained nodes, namely nodes inside composite states
fun allContainedNodes[]: set Nodes{HierarchicalStates.contains + Regions.contains}
