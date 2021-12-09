// About history nodes
module history_rules // Most constraints of history nodes, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

// This predication is optional
pred atMostOneDeepAndShallowHistory{
	all r1: Region | lone sh1: ShallowHistory | sh1 in r1.r_contains // In regions, there is at most one shallow history.
	all h1: HierarchicalState | lone sh1: ShallowHistory | sh1 in h1.h_contains // In composite states, there is at most one shallow history.
	all r1: Region| lone dh1: DeepHistory | dh1 in r1.r_contains // In regions, there is at most one deep history.
	all h1: HierarchicalState | lone dh1: DeepHistory | dh1 in h1.h_contains // In composite states, there is at most one deep history.
}

fact{
	// A history should be directed to a same or a deeper level which must contains at least one valid state for history to return, but definitely not to a level further outside, and should never be reached from (somewhere, possibly nested) inside their own compounds excluding start states	
	all h1: History, hs1: HierarchicalState | let n1 = getAllNodeInSameAndDeeperLevel[hs1] | 
		h1 in hs1.h_contains => (no h1.flowto_triggerwith[Name] || h1.flowto_triggerwith[Name] in n1)
						&& some (hs1.h_contains & (NormalState + CompositeState)) // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
						&& h1 not in (n1 - (StartState & hs1.h_contains)).flowto_triggerwith[Name] // History should never be reached from (somewhere, possibly nested) inside their own composite states excluding start states
	all h1: History, r1: Region | let n1 = getAllNodeInSameAndDeeperLevel[r1] | 
		h1 in r1.r_contains => (no h1.flowto_triggerwith[Name] || h1.flowto_triggerwith[Name] in n1)
						&& some (r1.r_contains & (NormalState + CompositeState)) // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
						&& h1 not in (n1 - (StartState & r1.r_contains)).flowto_triggerwith[Name] // History should never be reached from (somewhere, possibly nested) inside their own regions excluding start states	

	no h1: History | h1 not in HierarchicalState.h_contains + Region.r_contains // No history nodes are at the outermost level of a state diagram
	atMostOneDeepAndShallowHistory // In composite states and regions, there is at most one shallow history and at most one deep history
}
