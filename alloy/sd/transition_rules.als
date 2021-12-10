// About transition labels
module transition_rules // Most constraints of transition labels, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	all s1: States, t1: Names | lone s1.flowto_triggerwith[t1] // Transitions leaving one state can't have the same triggers
	// One state shouldn't be left with both a conditional and an unconditional transition
	all s1: States | some s1.flowto_triggerwith[EmptyTriggers] => no s1.flowto_triggerwith[NonEmptyTriggers]
	// If a composite state has a direct exit with a trigger, the trigger can't appear in the composite state(direct level)
	all c1: CompositeStates, t1: Names | 
		some c1.flowto_triggerwith[t1] 
			=> no (getAllNodesInSameAndDeeperLevel[c1] + getAllNodesInSameAndDeeperLevel[c1.inner]).flowto_triggerwith[t1]
}
