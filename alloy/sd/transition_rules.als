// About transition labels
module transition_rules // Most constraints of transition labels, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	all s1: State, t1: Name | lone s1.flowto_triggerwith[t1] // Transitions leaving one state can't have the same triggers
	// One state shouldn't be left with both a conditional and an unconditional transition
	all s1: State | some s1.flowto_triggerwith[EmptyName] => no s1.flowto_triggerwith[NonEmptyName]
	// If a composite state has a direct exit with a trigger, the trigger can't appear in the composite state(direct level)
	all c1: CompositeState, t1: Name | 
		some c1.flowto_triggerwith[t1] 
			=> no (getAllNodeInSameAndDeeperLevel[c1] + getAllNodeInSameAndDeeperLevel[c1.inner]).flowto_triggerwith[t1]
}
