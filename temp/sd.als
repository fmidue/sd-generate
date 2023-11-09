module GenUMLStateDiagram
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

      
pred flattenNonEmpty[theFrom : some (Nodes - EndNodes), flattened : some ProtoFlows, theTo : some (Nodes - StartNodes)]{
     (Flows & from.theFrom & to.theTo) in flattened
     flattened.from = theFrom
     flattened.to = theTo
}

// It implements true reachability
pred theFlatteningStrategy{
        // The following are predicates to implement "flattening".
        // It flattens flows from composite states to normal states and end nodes
        all pf: ProtoFlows | let sn = StartNodes - allContainedNodes |
                (pf.from in CompositeStates and pf.to in (NormalStates + EndNodes))
                        implies
                               (let newFrom = States & nodesInThis[pf.from] |
                                no newFrom implies no pf.derived
                                   else flattenNonEmpty[newFrom, pf.derived, pf.to])
                        else
                (pf.from in (sn + States) and pf.to in CompositeStates)
                        implies
                               (let newTo = from.(StartNodes & nodesInThis[pf.to]).to |
                                no newTo implies no pf.derived
                                   else flattenNonEmpty[pf.from, pf.derived, newTo])
                        else
                // It flattens flows from all states and the outermost start node to all states in regions, here end nodes are excluded, because coming to an end node means all end.
                (pf.from in (sn + States - nodesInThisAndDeeper[(Regions <: contains).(pf.to)]) and pf.to in (Regions.contains & States) and pf not in (to.(HistoryNodes + ForkNodes) + to.(nodesInOtherParallelRegions[(Regions <: contains).(pf.to)])).derived)
                        implies
                               (let newTo = from.(nodesInOtherParallelRegions[(Regions <: contains).(pf.to)] & StartNodes).to |
                                no newTo implies no pf.derived
                                   else flattenNonEmpty[pf.from, pf.derived, newTo])
                        else
                // It seems that above 3 predicates can constrain nodes except special nodes.
                // It flattens flows from all states and the outermost start node to fork nodes
                (pf.from in (sn + States) and pf.to in ForkNodes)
                        implies
                                flattenNonEmpty[pf.from, pf.derived, from.(pf.to).to + from.(nodesInOtherParallelRegions[(Regions <: contains).(from.(pf.to).to)] & StartNodes).to]
                        else
                // It flattens flows from states in regions to join nodes
                (pf.from in States & Regions.contains and pf.to in JoinNodes)
                        implies
                                flattenNonEmpty[pf.from, pf.derived, from.(pf.to).to]
                        else
                // The following 4 predicates flatten flows from states and the outermost start node to history nodes
                // When a history node is in a region and has no default flow (states + the outermost start state -> history nodes in regions and without a default flow)
                (pf.from in (sn + States) and pf.to in (HistoryNodes & Regions.contains) and no from.(pf.to))
                        implies
                               (let newTo = from.(StartNodes & ((RegionsStates <: contains).contains.(pf.to)).contains.contains).to |
                                no newTo implies no pf.derived
                                   else flattenNonEmpty[pf.from, pf.derived, newTo])
                        else
                // When a history node is in a region and has a default flow (states + the outermost start state -> history nodes in regions and with a default flow)
                (pf.from in (sn + States) and pf.to in (HistoryNodes & Regions.contains) and one from.(pf.to))
                        implies
                                flattenNonEmpty[pf.from, pf.derived, from.(pf.to).to + from.(StartNodes & nodesInOtherParallelRegions[contains.(pf.to)]).to]
                        else
                // When a history node is in a hierarchical state and has no default flow (states + the outermost start state -> history nodes in hierarchical states and without a default flow)
                (pf.from in (sn + States) and pf.to in (HistoryNodes & HierarchicalStates.contains) and no from.(pf.to))
                        implies
                               (let newTo = from.(StartNodes & (HierarchicalStates <: contains).(pf.to).contains).to |
                                no newTo implies no pf.derived
                                   else flattenNonEmpty[pf.from, pf.derived, newTo])
                        else
                // When a history node is in a hierarchical state and has a default flow (states + the outermost start state -> history nodes in hierarchical states and with a default flow)
                (pf.from in (sn + States) and pf.to in (HistoryNodes & HierarchicalStates.contains) and one from.(pf.to))
                        implies
                                flattenNonEmpty[pf.from, pf.derived, from.(pf.to).to]
                        else no pf.derived
}

      
// There is at most one start state in each level
pred atMostOneStartNodesInLevels{
        all r1: Regions | lone (StartNodes & r1.contains)  // In regions, there is at most one start state.
        all h1: HierarchicalStates | lone (StartNodes & h1.contains) // In hierarchical states, there is at most one start state.
        lone (StartNodes - allContainedNodes) // Outside all hierarchical states and regions, there is at most one start state
}

// Start states are only left by arrows pointing to something in their own compound state or deeper.
pred startNodesArrowsPointToSameOrDeeperLevels{
        all h1: HierarchicalStates, s1: StartNodes & h1.contains |
                (Flows <: from).s1.to in nodesInThisAndDeeper[h1] // When start states in hierarchical states
        all r1:Regions, s1: StartNodes & r1.contains |
                (Flows <: from).s1.to in nodesInThisAndDeeper[r1] // When start states in regions
}

fact{
        atMostOneStartNodesInLevels
        startNodesArrowsPointToSameOrDeeperLevels
}

      
// There is at most one end state in each level
pred atMostOneEndNodesInLevels{
        all r1: Regions | lone (EndNodes & r1.contains) // In regions, there is at most one end state.
        all h1: HierarchicalStates | lone (EndNodes & h1.contains) // In hierarchical states, there is at most one end state.
        lone (EndNodes - allContainedNodes) // Outside hierarchical states and regions, there is also at most one end state.
}

fact{
        atMostOneEndNodesInLevels
}

      
fact{
        // In a same region state, states in different region states can't be transited to each other.
        all rs1: RegionsStates, disj r1, r2: rs1.contains |
                disj [nodesInThisAndDeeper[r1], (Flows <: from).(nodesInThisAndDeeper[r2]).to]
}

      
// If two (or more) arrows leave the same such fork node, they must go to distinct parallel regions.
pred forkNodesGoToDistinctParallelRegions{
        all fn : ForkNodes | one rs : RegionsStates | (Flows <: from).fn.to in nodesInThisAndDeeper[rs]
                and no r : rs.contains, disj f1, f2 : (Flows <: from).fn |
                        (f1 + f2).to in nodesInThisAndDeeper[r]
}

// If two (or more) arrows enter the same such join node, they must come from distinct parallel regions.
pred joinNodesComeFromDistinctParallelRegions{
        all jn: JoinNodes | one rs: RegionsStates | (Flows <: to).jn.from in nodesInThisAndDeeper[rs]
                and no r : rs.contains, disj f1, f2 : (Flows <: to).jn |
                        (f1 + f2).from in nodesInThisAndDeeper[r]
}

fact{
        forkNodesGoToDistinctParallelRegions
        joinNodesComeFromDistinctParallelRegions
        disj [EndNodes, (Flows <: from).ForkNodes.to] // No transitions between end nodes and fork nodes (see example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample3.svg")
        disj [StartNodes + HistoryNodes, (Flows <: to).JoinNodes.from] // No transitions between start/history nodes and join nodes (It excludes the example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample2.svg")
        disj [ForkNodes + JoinNodes, (Flows <: from).(ForkNodes + JoinNodes).to] // No consecutive fork/join nodes
        all n1: ForkNodes & (Flows <: from).StartNodes.to |
                from.n1.label = EmptyTrigger // If a fork node is reached from a start state, it is not left by an arrow with non-empty transition label.
        all n1: (ForkNodes + JoinNodes) |
                to.n1.label = EmptyTrigger or from.n1.label = EmptyTrigger // No such node is both entered and left by arrows with non-empty transition label.
}

      
// Each composite state has at least one entry, except something like "box", in which all events happen, but it also has a default standard entry from the outermost start node which can be set invisible
pred atLeastOneEntryToCompositeStates{
        all c1: CompositeStates |
                let n1 = nodesInThisAndDeeper[c1], n2 = Nodes - c1 - n1 |
                        c1 in (Flows <: from).n2.to // Standard entry
                        or some (n1 & (Flows <: from).n2.to) // Direct, history or fork entry
}

// It implements only approximate reachability
pred approximateReachability{
        no derived
        // Each normal/fork/join/history/end state has at least one incoming arrow (from a start state or somewhere else)
        all n1: (Nodes - StartNodes - CompositeStates) | n1 in (Flows <: from).(Nodes - n1).to
        one (StartNodes - allContainedNodes) // Outside all hierarchical states and regions, there is exactly one start state
        atLeastOneEntryToCompositeStates
}

// set the flag representing if the start node is invisible
pred setStartNodesFlag{
        all s1: StartNodes | let h1 = CompositeStates - allContainedNodes |
        ({
                one h1
                no (Nodes - s1 - h1 - allContainedNodes)
                h1 = (Flows <: from).s1.to
        } or some h2: HierarchicalStates |
        {
                h2.contains = s1 + EndNodes
                (Flows <: from).s1.to = (EndNodes & h2.contains)
        }) implies s1.flag = 1 else s1.flag = 0
}


pred trueReachability{
        atLeastOneEntryToCompositeStates // It is a necessary condition for "true reachability"

        theFlatteningStrategy

        (Nodes - StartNodes - CompositeStates) in
                (StartNodes - allContainedNodes).^(~from.to) // Starting from a outermost start node except start nodes and composite states, all nodes except start nodes and composite states can be reachable
}

fact{
        setStartNodesFlag
        trueReachability
        // If there are history entries without default leaving transition, there must be a start state, because history nodes have neither record and a default leaving transition at the first entry
        all h1: HierarchicalStates, h2: HistoryNodes | let n1 = h1.contains |
                no (Flows <: from).h2 and h2 in (Flows <: from).(Nodes - n1).to
                        implies one (StartNodes & h1.contains)
        all r1: Regions, h1: HistoryNodes | let n1 = r1.contains |
                no (Flows <: from).h1 and h1 in (Flows <: from).(Nodes - n1).to
                        implies one (StartNodes & r1.contains)
        // If a composite state has regions and there are direct entries to one of the regions(except fork nodes), other regions must have start states
        all rs1: RegionsStates, disj r1, r2: rs1.contains |
                let n1 = nodesInThisAndDeeper[r1] |
                        some (n1 & (Flows <: from).(Nodes - n1 - ForkNodes).to)
                                implies one (StartNodes & r2.contains)
        // If a composite state with regions has a fork entry, those parallel regions without the entry from the fork node will contain a start node.
        all rs1: RegionsStates, r1: rs1.contains, f1: ForkNodes |
                some ((Flows <: from).f1.to & nodesInThisAndDeeper[rs1])
                and f1 not in rs1.contains.contains
                and no ((Flows <: from).f1.to & nodesInThisAndDeeper[r1])
                        implies one (StartNodes & r1.contains)
        // If a composite without regions has a standard entry, there must be a start state in it.
        all h1: HierarchicalStates & (Flows <: from).Nodes.to | one (StartNodes & h1.contains)
        // If a composite with regions has a standard entry, there must be a start state in each region.
        all rs1: RegionsStates & (Flows <: from).Nodes.to, r1: rs1.contains |
                 one (StartNodes & r1.contains)
}

      
// In a same level there shouldn't be two history nodes that are of the same type (deep/shallow). To some extent, this is imposed in order to appease PlantUML.
pred noDuplicateTypeHistoryNodes{
        all r1: Regions | lone (ShallowHistoryNodes & r1.contains)
                and lone (DeepHistoryNodes & r1.contains)
        all h1: HierarchicalStates | lone (ShallowHistoryNodes & h1.contains)
                and lone (DeepHistoryNodes & h1.contains)
}

pred noTransitionsBetweenHistoryNodesInSameLevel{
        all h1: HierarchicalStates, disj hn1, hn2: HistoryNodes & h1.contains |
                hn1 not in (Flows <: from).hn2.to
        all r1: Regions, disj hn1, hn2: HistoryNodes & r1.contains |
                hn1 not in (Flows <: from).hn2.to
}

fact{
        // A history should be directed to a same or a deeper level which must contains at least one valid state for history to return, but definitely not to a level further outside, and should never be reached from (somewhere, possibly nested) inside their own compounds excluding start states
        all hs1: HierarchicalStates, h1: HistoryNodes & hs1.contains |
                let n1 = nodesInThisAndDeeper[hs1] |
                {
                        (Flows <: from).h1.to in n1 // A history should be directed to a same or a deeper level
                        some (hs1.contains & States) // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
                        h1 not in (Flows <: from).(n1 - (StartNodes & hs1.contains)).to // History should never be reached from (somewhere, possibly nested) inside their own composite states excluding start nodes
                        h1 in (Flows <: from).(StartNodes & hs1.contains).to implies some (Flows <: from).h1 // If a start node in the same level points to a history node, that history node should probably have an outgoing flow.
                }
        all r1: Regions, h1: HistoryNodes & r1.contains |
                let n1 = nodesInThisAndDeeper[r1] |
                {
                        (Flows <: from).h1.to in n1 // A history should be directed to a same or a deeper level
                        some (r1.contains & States) // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
                        h1 not in (Flows <: from).(n1 - (StartNodes & r1.contains)).to // History should never be reached from (somewhere, possibly nested) inside their own regions excluding start nodes
                        h1 in (Flows <: from).(StartNodes & r1.contains).to implies some (Flows <: from).h1 // If a start node in the same level points to a history node, that history node should probably have an outgoing flow.
                }

        HistoryNodes in allContainedNodes // No history nodes are at the outermost level of a state diagram
        noDuplicateTypeHistoryNodes
        noTransitionsBetweenHistoryNodesInSameLevel // A history node should rather not point to another history node in the same level.
}

      
fact{
        all s1: States | no disj f1, f2: Flows | (f1 + f2).from = s1 and f1.label = f2.label // Transitions leaving one state can't have the same triggers
        // One state shouldn't be left with both a conditional and an unconditional transition
        all s1: States | let sl = from.s1.label | EmptyTrigger in sl implies sl = EmptyTrigger

        // If a composite state has a standard exit with a trigger, the trigger can't appear on leaving transitions from states in the composite state
        // If a composite state has a standard exit with an empty trigger, there are no leaving transitions from states in the composite state
        // If a composite state has a standard exit with non-empty triggers, the empty trigger can't appear on leaving transitions from states in the composite state
        all c1: CompositeStates |
                EmptyTrigger in from.c1.label
                        implies
                                no (Flows <: from).(nodesInThisAndDeeper[c1] & States)
                        else
               some from.c1.label
                        implies
                                disj[from.c1.label + EmptyTrigger, from.(nodesInThisAndDeeper[c1] & States).label]
}

      
fact{
        // No compound or region may be empty or contain only history/fork/join nodes.
        some (States + EndNodes) // It constrains that the whole model can't be empty or contain only only history/fork/join nodes.
        all h1: HierarchicalStates | some (h1.contains & (States + EndNodes)) // It constrains all composite states without regions
        all r1: Regions | some (r1.contains & (States + EndNodes)) // It constrains all region levels
}

      
// Entities which are "neighbours" (in the sense of living directly side by side in the same compound or region, but not in two parallel regions of the same compound or such), they must not have the same name. (ignored empty names)
pred noSameNamesInSameLevels{
        all h1: HierarchicalStates, disj s1, s2: h1.contains | disj[s1.name, s2.name] // In a composite state without regions, no states have same names
        all r1: Regions, disj s1, s2: r1.contains | disj[s1.name, s2.name] // In a region, no states have same names
        all disj s1, s2: States - allContainedNodes | disj[s1.name, s2.name] // In the outermost level, no states have same names
        all rs1: RegionsStates, disj r1, r2: rs1.contains | disj[r1.name, r2.name] // In a composite state, no regions have same names
}

// In a compound or region, the name of the outermost level must not be repeated anywhere deeper inside. (ignored empty names)
pred outermostLevelsNamesNotInDeeperLevels{
        all h1: HierarchicalStates, s1: nodesInThisAndDeeper[h1] | disj[h1.name, s1.name]  // For all states deeper inside a composite state without regions
        all r1: Regions, s1: nodesInThisAndDeeper[r1] | disj[r1.name, s1.name] // For all states deeper inside a region
        all h1: HierarchicalStates, r1: regionsInThisAndDeeper[h1] | disj[h1.name, r1.name] // For all regions deeper inside a composite state without regions
        all r1: Regions, r2: regionsInThisAndDeeper[r1] | disj[r1.name, r2.name] // For all regions deeper inside a region
}

// About names of states (and of regions etc.)
fact{
        noSameNamesInSameLevels
        outermostLevelsNamesNotInDeeperLevels
}


run {} for 6 but 3 Int, 0 RegionsStates, 2 HierarchicalStates,
0 Regions, 3 NormalStates, 9 ComponentNames, 1 EndNodes,
0 ForkNodes, 0 JoinNodes, 0 HistoryNodes
    