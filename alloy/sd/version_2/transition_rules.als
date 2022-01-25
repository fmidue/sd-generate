// About transition labels
module transition_rules // Most constraints of transition labels, but some constraints are directly with the signatures

open components_sig as components // import all signatures

fact{
        all s1: States | no disj f1, f2: Flows | (f1 + f2).from = s1 and f1.label = f2.label // Transitions leaving one state can't have the same triggers
        // One state shouldn't be left with both a conditional and an unconditional transition
        all s1: States | let sl = from.s1.label | EmptyTrigger in sl implies sl = EmptyTrigger
        // If a composite state has a standard exit with a trigger, the trigger can't appear on leaving transitions from states in the composite state
        all c1: CompositeStates | disj[from.c1.label, from.(nodesInThisAndDeeper[c1] & States).label]
        // If a composite state has a standard exit with an empty trigger, non-empty triggers can't appear on leaving transitions from states in the composite state
        // If a composite state has a standard exit with non-empty triggers, the empty trigger can't appear on leaving transitions from statesin the composite state
        all c1: CompositeStates | (EmptyTrigger in from.c1.label
                implies disj[TriggerNames, from.(nodesInThisAndDeeper[c1] & States).label])
                and (some (TriggerNames & from.c1.label)
                implies EmptyTrigger not in from.(nodesInThisAndDeeper[c1] & States).label)
}