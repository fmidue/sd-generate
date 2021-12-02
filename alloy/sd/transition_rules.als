// About transition labels
module transition_rules // most constraints of transition labels, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	one t1: Trigger | #t1.notated  = 0 // No need to have many unconditional triggers to express unconditional transitions
	all s1: State, t1: Trigger | s1 in Node.flowfrom_triggerwith[t1] => #s1.flow_triggerwith[t1] = 0 else #s1.flow_triggerwith[t1] < 2// Transitions leaving one state can't have the same triggers

	// One state shouldn't be left with both a conditional and an unconditional transition
	all s1: State, t1, t2: Trigger | (t1.notated = none && t1 != t2 && s1.flow_triggerwith[t1] != none) => s1.flow_triggerwith[t2] = none && s1 not in Node.flowfrom_triggerwith[t2]
	all s1: State, n1: Node, t1, t2: Trigger | (s1 in n1.flowfrom_triggerwith[t1] && t1.notated = none && t1 != t2) => s1.flow_triggerwith[t2] = none && s1 not in (Node - n1).flowfrom_triggerwith[t2]
}
