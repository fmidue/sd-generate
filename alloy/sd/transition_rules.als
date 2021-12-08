// About transition labels
module transition_rules // Most constraints of transition labels, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	lone t1: Trigger | no t1.notated // No need to have many unconditional triggers to express unconditional transitions
	all s1: State, t1: Trigger | lone s1.flowto_triggerwith[t1] // Transitions leaving one state can't have the same triggers
	// One state shouldn't be left with both a conditional and an unconditional transition
	all s1: State, t1: Trigger | no t1.notated && some s1.flowto_triggerwith[t1] => no s1.flowto_triggerwith[Trigger - t1]
	// If a composite state has a direct exit with a certain label, the label can't appear in the composite state(direct level)
	all c1: CompositeState, t1: Trigger | some c1.flowto_triggerwith[t1] => no (getAllNodeInSameAndDeeperLevel[c1] + getAllNodeInSameAndDeeperLevel[c1.inner]).flowto_triggerwith[t1]
	// If a composite state has a direct uncondiontal exit, no other transitions allowed in in the composite state(direct level)
	all c1: CompositeState, t1: Trigger | no t1.notated && some c1.flowto_triggerwith[t1] => no (getAllNodeInSameAndDeeperLevel[c1] + getAllNodeInSameAndDeeperLevel[c1.inner]).flowto_triggerwith
}
