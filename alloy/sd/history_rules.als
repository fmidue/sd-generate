// About history nodes
module history_rules // Most constraints of history nodes, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

// This predication is optional
pred atMostOneDeepAndShallowHistoryNodes{
	all r1: Regions | lone sh1: ShallowHistoryNodes | sh1 in r1.r_contains // In regions, there is at most one shallow history.
	all h1: HierarchicalStates | lone sh1: ShallowHistoryNodes | sh1 in h1.h_contains // In composite states, there is at most one shallow history.
	all r1: Regions | lone dh1: DeepHistoryNodes | dh1 in r1.r_contains // In regions, there is at most one deep history.
	all h1: HierarchicalStates | lone dh1: DeepHistoryNodes | dh1 in h1.h_contains // In composite states, there is at most one deep history.
}

fact{
	// A history should be directed to a same or a deeper level which must contains at least one valid state for history to return, but definitely not to a level further outside, and should never be reached from (somewhere, possibly nested) inside their own compounds excluding start states	
	all h1: HistoryNodes, hs1: HierarchicalStates | let n1 = getAllNodesInSameAndDeeperLevels[hs1] | 
		h1 in hs1.h_contains => 
			(no h1.flow[Triggers] || h1.flow[Triggers] in n1) && 
			some (hs1.h_contains & (NormalStates + CompositeStates)) && // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
			h1 not in (n1 - (StartStates & hs1.h_contains)).flow[Triggers] // History should never be reached from (somewhere, possibly nested) inside their own composite states excluding start states
	all h1: HistoryNodes, r1: Regions | let n1 = getAllNodesInSameAndDeeperLevels[r1] | 
		h1 in r1.r_contains =>
			(no h1.flow[Triggers] || h1.flow[Triggers] in n1) &&
			some (r1.r_contains & (NormalStates + CompositeStates)) && // It excludes "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample5.svg"
			h1 not in (n1 - (StartStates & r1.r_contains)).flow[Triggers] // History should never be reached from (somewhere, possibly nested) inside their own regions excluding start states	

	no h1: HistoryNodes | h1 not in HierarchicalStates.h_contains + Regions.r_contains // No history nodes are at the outermost level of a state diagram
	atMostOneDeepAndShallowHistoryNodes // In composite states and regions, there is at most one shallow history and at most one deep history
}
