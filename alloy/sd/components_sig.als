module components_sig // All signatures and some direct constraints in this module

// ProtoFlows includes actual flows and translated flows
sig ProtoFlows{
        from: one (Nodes - EndNodes), // End nodes have no leaving transitions
        to: one (Nodes - StartNodes), // Start nodes have no coming transitions
        derived: set ProtoFlows
}

// Flows is corresponding to actual flows
sig Flows extends ProtoFlows{
        label: one Triggers
}

fact{
        (ProtoFlows - Flows) in ProtoFlows.derived // No independent derived flows
        no disj pf1, pf2: ProtoFlows | pf1.from = pf2.from and pf1.to = pf2.to and lone (pf1.label + pf2.label) // no duplicate flows
}

// All components are a node, this is a super class
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

abstract sig StartNodes extends Nodes{
        flag: one Int // It is used to check if this can be neglected
}
{
        flag = 0 or flag = 1 // If flag = 0, it can't be neglected, or it can be neglected
        // Start nodes are left by one unconditionally transition
        one (Flows <: from).this // Start nodes have only one leaving transition
        from.this.label = EmptyTrigger // Start states are not left by an arrow with non-empty transition label
}

abstract sig EndNodes extends Nodes{}

abstract sig NormalStates extends States{}

abstract sig Regions{
        name: lone ComponentNames, // Regions have at most one name
        contains: disj some Nodes
}
{
        this in RegionsStates.contains // No regions exist independently
        contains in (StartNodes + EndNodes) implies no (Flows <: from).(RegionsStates <: contains).this // If a region has only a start state or a end state, a leaving transition is superfluous for its regions state
}

// Composite states: HierarchicalState + RegionsState
abstract sig CompositeStates extends States{}

// HierarchicalState: Composite states without regions
abstract sig HierarchicalStates extends CompositeStates{
        contains: disj some Nodes
}
{
        contains in (StartNodes + EndNodes) implies no (Flows <: from).this // If a hierarchical state has only a start state or a end state, a leaving transition is superfluous.
}

// RegionState: Composite states with regions
abstract sig RegionsStates extends CompositeStates{
        contains: disj some Regions // Regions are in region states
}
{
        no name // Region states have no name themselves
        not (one contains) // There can't be only one region in Region states
}

// A composite state can't appear in in deeper level of itself
pred acyclicContain{
        all c1: CompositeStates | c1 not in nodesInThisAndDeeper[c1] // A composite state can't appear in deeper level of itself
}

fact{
        acyclicContain
        disj[Regions.contains, HierarchicalStates.contains] // Regions and hierarchical states can't contain same nodes.
}

abstract sig ForkNodes extends Nodes{}
{
        // It should be 1 to n(n > 1), n to n is not allowed
        not (lone (Flows <: from).this)  // Each fork node has two or more leaving arrows
        one (Flows <: to).this // It constrains the number of coming transition = 1
        one from.this.label // For fork nodes, leaving transitions should all have same conditions
}

abstract sig JoinNodes extends Nodes{}
{
        // It should be n(n >= 2) to 1, n to n is not allowed
        one (Flows <: from).this // It constrains the number of leaving transition = 1
        not (lone (Flows <: to).this) // Each join node has two or more entering arrows
        one to.this.label // For join nodes, coming transitions should all have same conditions
}

// HistoryNodes: ShallowHistoryNodes + DeepHistoryNodes
abstract sig HistoryNodes extends Nodes{}
{
        // History nodes are left by at most one unconditionally leaving transition
        lone (Flows <: from).this // At most one leaving transition
        from.this.label in EmptyTrigger // The leaving transition of history shouldn't have conditions
        this not in (Flows <: from).this.to // No self-loop transition
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
