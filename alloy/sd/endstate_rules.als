// About end states
module endstate_rules // most constraints of end states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

fact{
	all r1: Region, e1: EndState | e1 in r1.contains => lone e1 // In regions, there is at most one end state.
	all c1: CompositeState, e1: EndState | e1 in c1.contains => lone e1 // In composite states, there is at most one end state.
	all e1: EndState | e1 not in (CompositeState.contains + Region.contains) => lone e1 // Outside composite states and regions, there is also at most one end state.
	all e1: EndState | e1 in Node.flowto_triggerwith[Trigger] // If end states exists, there must be transitions to them, but no transitions between end states and fork nodes (see example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample3.svg") 
}
