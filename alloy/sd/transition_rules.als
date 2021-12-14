// About transition labels
module transition_rules // Most constraints of transition labels, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	all s1: States, t1: Triggers | lone s1.flow[t1] // Transitions leaving one state can't have the same triggers
	// One state shouldn't be left with both a conditional and an unconditional transition
	all s1: States | one s1.flow[EmptyTrigger] => no s1.flow[TriggerNames]
	// If a composite state has a direct exit with a trigger, the trigger can't appear in the composite state(direct level)
	all c1: CompositeStates, t1: Triggers | 
		some c1.flow[t1] => no allNodesInThisAndDeeper[c1].flow[t1]
}
