module version1 // All nodes and naming rules are introduced, but no transitions and further strategies.

open components_sig as components
open startstate_rules // import constraints of start states
open endstate_rules // import constraints of end states
open substate_rules // import constraints of "substates"
open name_rules // import constraints of names

// A composite state can't appear in in deeper level of itself
pred acyclicContain{
        all c1: CompositeStates | c1 not in nodesInThisAndDeeper[c1]
}

fact{
        acyclicContain
        disj[Regions.contains, HierarchicalStates.contains] // Regions and hierarchical states can't contain same nodes.
}

run {} for 3 but exactly 1 RegionsStates, exactly 1 HierarchicalStates, exactly 2 Regions, exactly 2 NormalStates, exactly 4 ComponentNames
