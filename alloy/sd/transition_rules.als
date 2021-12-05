// About transition labels
module transition_rules // Most constraints of transition labels, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	lone t1: Trigger | #t1.notated  = 0 // No need to have many unconditional triggers to express unconditional transitions
	all s1: State, t1: Trigger | #s1.flowto_triggerwith[t1] < 2// Transitions leaving one state can't have the same triggers

	// One state shouldn't be left with both a conditional and an unconditional transition
	all s1: State, t1: Trigger | (t1.notated = none && s1.flowto_triggerwith[t1] != none) => s1.flowto_triggerwith[Trigger - t1] = none
}
