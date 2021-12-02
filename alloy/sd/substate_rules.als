// About "substates"
module substate_rules // most constraints of "substates", but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	// No compound or region may be empty or contain only history/fork/join nodes.
	State & (NormalState + CompositeState) != none // It constrains outermost level
	all c1: CompositeState | #c1.inner = 0 => c1.contains & (NormalState + CompositeState) != none // It constrains all compound levels
	all r1: Region | r1.contains & (NormalState + CompositeState) != none // It constrains all region levels
}
