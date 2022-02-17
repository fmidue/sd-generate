module teaching_examples // It is used to generate specific teaching examples

open uml_state_diagram

// It is not necessary, but in order to generate some both manifold and non-boring SDs, we can add it.
pred atLeastOneExitFromCompositeStates{
        all c1: CompositeStates |
                let n1 = nodesInThisAndDeeper[c1], n2 = Nodes - n1 |
                        n2 in (Flows <: from).c1.to // Standard exit
                        or some (n2 & (Flows <: from).n1.to) // Direct or join exit
}

// No regions, no history nodes
pred scenario1{
        no HistoryNodes
        no Regions
        no EndNodes
        no s : NormalStates | no s.name
        no disjoint s1,s2 : NormalStates | s1.name = s2.name
        EmptyTrigger not in from.States.label
        all s : States | some (Flows <: from).s
        one HierarchicalStates
        let hs = HierarchicalStates, inner = hs + hs.contains |
                some ((Flows <: from).hs.to & (Nodes - inner))
                and mul[2,#inner] >= #Nodes
                and no hs.name
        #Nodes >= 8
}

// Some regions, no history nodes
pred scenario2{
        no HistoryNodes
        #Regions = 2
        one EndNodes
        EndNodes not in allContainedNodes
        no s : NormalStates | no s.name
        no disjoint s1,s2 : NormalStates | s1.name = s2.name
        EmptyTrigger not in from.States.label
        all s : NormalStates | some (Flows <: from).s
        no HierarchicalStates
        some (ForkNodes + JoinNodes)
        let inner = RegionsStates + Regions.contains |
                some ((Flows <: from).inner.to & (NormalStates - inner))
        mul[2,#Regions.contains] >= #Nodes
        no Regions.name
        #Nodes >= 8
}

// No regions, some history nodes
pred scenario3{
        some HistoryNodes
        no Regions
        # Nodes > 8
        # Flows > 8
        some HierarchicalStates
        atLeastOneExitFromCompositeStates
}

// run scenario1 for 10 but 6 Int, 0 EndNodes, 0 Regions, 0 RegionsStates, 0 ForkNodes, 0 JoinNodes, 0 HistoryNodes, exactly 1 HierarchicalStates

run scenario2 for 15 but 6 Int, exactly 1 EndNodes, exactly 2 Regions, 0 HierarchicalStates, exactly 1 RegionsStates, 1 ForkNodes, 1 JoinNodes, 0 HistoryNodes
