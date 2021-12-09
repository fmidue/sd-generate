// About end states
module endstate_rules // Most constraints of end states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

fact{
	all r1: Region | lone (EndState & r1.r_contains) // In regions, there is at most one end state.
	all h1: HierarchicalState | lone (EndState & h1.h_contains) // In hierarchical states, there is at most one end state.
	lone e1: EndState | e1 not in (HierarchicalState.h_contains + Region.r_contains) // Outside hierarchical states and regions, there is also at most one end state.
	all e1: EndState | e1 in (Node - ForkNode).flowto_triggerwith[Name] // If end states exists, there must be transitions to them, but no transitions between end states and fork nodes (see example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample3.svg") 
}
