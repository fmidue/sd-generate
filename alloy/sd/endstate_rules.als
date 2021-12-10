// About end states
module endstate_rules // Most constraints of end states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

fact{
	all r1: Regions | lone (EndStates & r1.r_contains) // In regions, there is at most one end state.
	all h1: HierarchicalStates | lone (EndStates & h1.h_contains) // In hierarchical states, there is at most one end state.
	lone e1: EndStates | e1 not in (HierarchicalStates.h_contains + Regions.r_contains) // Outside hierarchical states and regions, there is also at most one end state.
}
