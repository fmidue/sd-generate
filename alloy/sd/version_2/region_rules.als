// About regions and region states
module region_rules // Most constraints of regions and region states, but some constraints are directly with the signature

open components_sig as components // import all signatures

fact{
        // In a same region state, states in different region states can't be transited to each other
        all rs1: RegionsStates, disj r1, r2: rs1.contains | noCrossing [r1, r2]
                disj [nodesInThisAndDeeper[r1], from.(nodesInThisAndDeeper[r2]).to]
}
