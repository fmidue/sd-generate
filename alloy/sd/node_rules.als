// About fork/join nodes
module node_rules // Most constraints of fork and join nodes, but some constraints are directly with the signatures

open components_sig as components // import all signatures

// If two (or more) arrows leave the same such fork node, they must go to distinct parallel regions.
pred forkNodesGoToDistinctParallelRegions{
        all fn : ForkNodes | one rs : RegionsStates | (Flows <: from).fn.to in nodesInThisAndDeeper[rs]
                and no r : rs.contains, disj f1, f2 : (Flows <: from).fn |
                        (f1 + f2).to in nodesInThisAndDeeper[r]
}

// If two (or more) arrows enter the same such join node, they must come from distinct parallel regions.
pred joinNodesComeFromDistinctParallelRegions{
        all jn: JoinNodes | one rs: RegionsStates | (Flows <: to).jn.from in nodesInThisAndDeeper[rs]
                and no r : rs.contains, disj f1, f2 : (Flows <: to).jn |
                        (f1 + f2).from in nodesInThisAndDeeper[r]
}

// Approximation: the sources of a join can be active at the same time, to avoid obvious unreachability.
// A join is accepted if its sources are non-interfering, or if all but at most one of them are controllable.
pred joinSourcesCanBeActiveTogether {
    all jn: JoinNodes |
        let arms = (Flows <: to).jn.from,
            rs = { c: RegionsStates | arms in nodesInThisAndDeeper[c]
                        and no r: c.contains | arms in nodesInThisAndDeeper[r] } |
        // Non-interfering: no source is left by a trigger that is also used in the other regions
        (all s: arms |
            let sib = nodesInThisAndDeeper[rs]
                        - nodesInThisAndDeeper[{ r: rs.contains | s in nodesInThisAndDeeper[r] }] |
                no ( (((Flows & from.s) - to.jn).label - EmptyTrigger) & ((Flows & from.sib).label - EmptyTrigger) ))
        or
        // Sufficient condition for simultaneity: all sources but at most one (s0) are "controllable", i.e. from every state of its region
        // the source can be reached using only unconditional transitions and triggers private to that region (not used in the other regions).
        // Then: let the region of s0 reach s0, afterwards steer each other region to its source; private triggers cannot disturb other regions.
        // Paths only run over states: history nodes (non-deterministic target), nested joins and forks are not used (rarely used in practice).
        lone { s: arms |
            let inR = nodesInThisAndDeeper[{ r: rs.contains | s in nodesInThisAndDeeper[r] }],
                st = inR & (States + EndNodes),
                free = (((Flows & from.inR & to.inR).label - EmptyTrigger) - (Flows & from.(nodesInThisAndDeeper[rs] - inR)).label) + EmptyTrigger,
                ff = Flows & label.free,
                move = (~(ff <: from)).(ff <: to),
                child = (HierarchicalStates <: contains) + (RegionsStates <: contains).(Regions <: contains),
                step = st <: move :> st,                                                                              // transitions inside the region
                enter = (st & CompositeStates) <: (child :> StartNodes).(~(Flows <: from)).(Flows <: to) :> st,      // entering a composite state goes on to the target of its start node
                leave = st <: ~^child.(CompositeStates <: move) :> st |                                               // leaving a composite state from anywhere inside it
            not ((NormalStates + EndNodes) & inR in s.*~(step + enter + leave)) }                                     // s is not controllable
}

fact{
        forkNodesGoToDistinctParallelRegions
        joinNodesComeFromDistinctParallelRegions
        joinSourcesCanBeActiveTogether
        disj [EndNodes, (Flows <: from).ForkNodes.to] // No transitions between end nodes and fork nodes (see example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample3.svg")
        disj [StartNodes + HistoryNodes, (Flows <: to).JoinNodes.from] // No transitions between start/history nodes and join nodes (It excludes the example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/MyExample2.svg")
        disj [ForkNodes + JoinNodes, (Flows <: from).(ForkNodes + JoinNodes).to] // No consecutive fork/join nodes
        all n1: ForkNodes & (Flows <: from).(StartNodes + HistoryNodes).to |
                from.n1.label = EmptyTrigger // If a fork node is reached from a start state or history node, it is not left by an arrow with non-empty transition label.
        all n1: (ForkNodes + JoinNodes) |
                to.n1.label = EmptyTrigger or from.n1.label = EmptyTrigger // No such node is both entered and left by arrows with non-empty transition label.
}
