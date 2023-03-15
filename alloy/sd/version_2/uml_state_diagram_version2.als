module uml_state_diagram_version2 // Show uml state diagrams
/*
  except the reachability strategy and the visiability flag of StartNodes and some additional treatment like:
  1. History nodes should be in composite states/regions
  2. Some special examples should be excluded.
*/

open components_sig as components // import all signatures
open startstate_rules // import constraints of start states
open endstate_rules // import constraints of end states
open region_rules // import constraints of regions and region states
open node_rules // import constraints of fork and join nodes
open history_rules // import constraints of history nodes
open transition_rules // import constraints of transition labels
open substate_rules // import constraints of "substates"
open name_rules // import constraints of names

// A composite state can't appear in in deeper level of itself
pred acyclicContain{
        all c1: CompositeStates | c1 not in nodesInThisAndDeeper[c1] // A composite state can't appear in deeper level of itself
}

fact{
        acyclicContain
        disj[Regions.contains, HierarchicalStates.contains] // Regions and hierarchical states can't contain same nodes.
}

run {} for 6 but exactly 2 HierarchicalStates, exactly 1 EndNodes
