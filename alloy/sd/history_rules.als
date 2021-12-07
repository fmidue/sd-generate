// About history nodes
module history_rules // Most constraints of history nodes, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

// This predication is optional
pred atMostOneDeepAndShallowHistory{
	all r1: Region | lone h1: ShallowHistory | h1 in r1.contains // In regions, there is at most one shallow history.
	all c1: CompositeState| lone h1: ShallowHistory | h1 in c1.contains // In composite states, there is at most one shallow history.
	all r1: Region| lone h1: DeepHistory | h1 in r1.contains // In regions, there is at most one deep history.
	all c1: CompositeState| lone h1: DeepHistory | h1 in c1.contains // In composite states, there is at most one deep history.
}

fact{
	// A history should be directed to a same or a deeper level which must contains at least one valid state for history to return, but definitely not to a level further outside, and should never be reached from (somewhere, possibly nested) inside their own compounds excluding start states	
	all h1: History, c1: CompositeState | let n1 = getAllNodeInSameAndDeeperLevel[c1] | h1 in c1.contains => (h1.flowto_triggerwith[Trigger] = none || h1.flowto_triggerwith[Trigger] in n1)
																			&& c1.contains & (NormalState + CompositeState) != none // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
																			&& h1 not in (n1 - (StartState & c1.contains)).flowto_triggerwith[Trigger] // History should never be reached from (somewhere, possibly nested) inside their own composite states excluding start states
	all h1: History, r1: Region | let n1 = getAllNodeInSameAndDeeperLevel[r1] | h1 in r1.contains => (h1.flowto_triggerwith[Trigger] = none || h1.flowto_triggerwith[Trigger] in n1)
																	&& r1.contains & (NormalState + CompositeState) != none // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
																	&& h1 not in (n1 - (StartState & r1.contains)).flowto_triggerwith[Trigger] // History should never be reached from (somewhere, possibly nested) inside their own regions excluding start states	

	no h1: History | h1 not in CompositeState.contains + Region.contains // No history nodes are at the outermost level of a state diagram
	atMostOneDeepAndShallowHistory // In composite states and regions, there is at most one shallow history and at most one deep history
}
