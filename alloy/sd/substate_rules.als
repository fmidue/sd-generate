// About "substates"
module substate_rules // Most constraints of "substates", but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	// No compound or region may be empty or contain only history/fork/join nodes.
	State & (NormalState + CompositeState + EndState) != none // It constrains outermost level
	all c1: CompositeStateWithoutRegion | c1.contains & (NormalState + CompositeState + EndState) != none // It constrains all composite states without regions
	all r1: Region | r1.contains & (NormalState + CompositeState + EndState) != none // It constrains all region levels
}
