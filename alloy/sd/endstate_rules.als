// About end states
module endstate_rules // Most constraints of end states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

fact{
	all r1: Region | #(EndState & r1.contains) < 2 // In regions, there is at most one end state.
	all c1: CompositeState | #(EndState & c1.contains) < 2 // In composite states, there is at most one end state.
	lone e1: EndState | e1 not in (CompositeState.contains + Region.contains) // Outside composite states and regions, there is also at most one end state.
	all e1: EndState | e1 in (Node - ForkNode).flowto_triggerwith[Trigger] // If end states exists, there must be transitions to them, but no transitions between end states and fork nodes (see example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample3.svg") 
}
