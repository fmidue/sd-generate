// About end states
module endstate_rules // most constraints of end states, but some constraints are directly with the signature 

open components_sig as components // import all signatures

fact{
	all r1: Region, e1: EndState | e1 in r1.contains => #e1 < 2 // In regions, there is at most one end state.
	all c1: CompositeState, e1: EndState | e1 in c1.contains => #e1 < 2 // In composite states, there is at most one end state.
	all e1: EndState | e1 not in (CompositeState.contains + Region.contains) => #e1 < 2 // Outside composite states and regions, there is also at most one end state.
	all e1: EndState | e1 in (State.flow_triggerwith[Trigger] + Node.flowto_triggerwith[Trigger]) // If end states exists, there must be transitions to them
			    && e1 not in Node.flowfrom_triggerwith[Trigger] // No leaving transitions(including to nodes), only coming transitions,
			    && e1 not in ForkNode.flowto_triggerwith[Trigger] // No transitions between end states and fork nodes
}
