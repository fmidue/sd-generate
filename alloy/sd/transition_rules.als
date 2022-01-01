// About transition labels
module transition_rules // Most constraints of transition labels, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	all s1: States | no disj f1, f2: Flows | (f1 + f2).to = s1 && f1.label = f2.label // Transitions leaving one state can't have the same triggers
	// One state shouldn't be left with both a conditional and an unconditional transition
	all s1: States | let sl = from.s1.label | 
		EmptyTrigger in sl implies TriggerNames not in sl 
	// If a composite state has a direct exit with a trigger, the trigger can't appear in the composite state(direct level)
	all c1: CompositeStates, t1: Triggers | t1 in from.c1.label 
		implies t1 not in  from.(nodesInThisAndDeeper[c1]).label
}
