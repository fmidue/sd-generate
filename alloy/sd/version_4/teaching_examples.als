module teaching_examples // It is used to generate specific teaching examples

open uml_state_diagram

// No regions, no history nodes
pred scenario1{
        no HistoryNodes
        no Regions
        # Nodes > 8
        # Flows > 8
        // some HierarchicalStates
}

// Some regions, no history nodes
pred scenario2{
        no HistoryNodes
        some Regions
        # Nodes > 8
        # Flows > 8
        // some HierarchicalStates
        // some ForkNodes
        // some JoinNodes
}

// No regions, some history nodes
pred scenario3{
        some HistoryNodes
        no Regions
        # Nodes > 8
        # Flows > 8
        // some HierarchicalStates
}

run scenario3 for 10 but 6 Int
