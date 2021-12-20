open uml_state_diagram

abstract sig Flows{
	from: one (Nodes - EndNodes),
	to: one (Nodes - StartNodes),
	derived: disj set TranslatedFlows
}

abstract sig OriginalFlows extends Flows{}

abstract sig TranslatedFlows extends Flows{}

// It gets all nodes in other parallel regions
fun nodesInOtherParallelRegions[rs: set Regions]: set Nodes{
	((contains.rs).(RegionsStates <: contains) - rs).contains
}

// It gets contained nodes in a direct level of composite states
fun nodesInThis[c1: CompositeStates] : set Nodes {
    c1.(HierarchicalStates <: contains) + c1.(RegionsStates <: contains).(Regions <: contains)
}

pred trueReachability{
	atLeastOneEntryToCompositeStates // It is a necessary condition for "true reachability"
	no disj f1, f2: Flows | f1.from = f2.from and f1.to = f2.to // no duplicate flows

	all n1, n2: Nodes | n2 in n1.flow[Triggers] 
		implies (one of1: OriginalFlows | of1.from = n1 and of1.to = n2) 
		else (no of2: OriginalFlows | of2.from = n1 and of2.to = n2) // It gets all original flows

	all tf: TranslatedFlows | tf in Flows.derived and tf.from in (States + StartNodes - (StartNodes & allContainedNodes)) // It restricts the root of a multiple flattening is a state or a outermost start state
	
	all of: OriginalFlows | 
		(of.from in (HistoryNodes + JoinNodes + ForkNodes + (StartNodes & allContainedNodes)) // Because I want the root of a multiple flattening is a state or a outermost start state, these original flows have no derived translated flows.
		or (of.from in (NormalStates + (StartNodes - (StartNodes & allContainedNodes)))	
		     and of.to in ((NormalStates + EndNodes) - (NormalStates & Regions.contains))))
			implies no of.derived // all original flows among the outermost start node and normal states not in regions have no derivedd translated flows, and all original flows from the outermost start node and normal states not in regions to end nodes also have no derivedd translated flows.
	
	all tf: TranslatedFlows | (tf.from + tf.to) in (NormalStates + StartNodes + EndNodes) implies no tf.derived // It may be implicted by other constraints
	
	// The following are predicates to implement "flattening". Special nodes including history/fork/join nodes will be processed individually
	// It flattens flows from composite states to normal states and end nodes
	all f: Flows | (f.from in CompositeStates and f.to in (NormalStates + EndNodes))
   		implies let cs = States & nodesInThis[f.from] |
    			((all s: cs | one nf: f.derived | nf.from = s and nf.to = f.to)
    			and (all nf : f.derived | nf.from in cs and nf.to = f.to)) 
	// It flattens flows from all states and the outermost start node to composite states
	all f: Flows | (f.from in (States + StartNodes - (StartNodes & allContainedNodes)) and f.to in CompositeStates)
   		implies let cs = (StartNodes & nodesInThis[f.to]).flow[Triggers] |
    			((all s: cs | one nf: f.derived | nf.from = f.from and nf.to = s)
    			and (all nf : f.derived | nf.from = f.from and nf.to in cs))

	// It flattens flows from all states in regions to all states and end nodes
	/* I think this situation can be simplified. We don't need to flatten all translated flows in other paralell regions, because they have no impact on reachability judgement. 
	/*all f: Flows | (f.from in (Regions.contains & States) and f.to in (States + EndNodes))
		implies let cs = nodesInOtherParallelRegions[f.from.contains] |
			((all s: cs | one nf: f.derived | nf.from = s and nf.to = f.to)
			and (all nf: f.derived | nf.from in cs and nf.to = f.to)) */

	// It flattens flows from all states and the outermost start node to all states in regions, here end nodes are excluded, because coming to an end node means all end.  
	all f: Flows | (f.from in (States + StartNodes - (StartNodes & allContainedNodes)) and f.to in (Regions.contains & States))
		implies let cs = (nodesInOtherParallelRegions[(Regions <: contains).(f.to)] & StartNodes).flow[Triggers] |
			((all s: cs | one nf: f.derived | nf.from = f.from and nf.to = s)
			and (all nf: f.derived | nf.from = f.from and nf.to in cs))
	// It seems that above 4 predicates can constrain nodes except special nodes.

	// It flattens flows from all states and the outermost start node to fork nodes
	all f: Flows, fn: ForkNodes | f.from in (States + StartNodes - (StartNodes & allContainedNodes)) and f.to = fn
		implies let cs = fn.flow[Triggers] + (nodesInOtherParallelRegions[(Regions <: contains).(fn.flow[Triggers])] & StartNodes).flow[Triggers] |  
			((all s: cs | one nf: f.derived | nf.from = f.from and nf.to = s)
			and (all nf: f.derived | nf.from = f.from and nf.to in cs))
	
	// It flattens flows from join nodes to all states and the outermost start node
	all f: Flows, jn: JoinNodes | f.from in (States & Regions.contains) and f.to = jn
		implies let cs = f.from | // Here, I omit flattening all states in other parallel regions, because it doesn't have impact on judging reachability, but if it is added, it will increase considerable complexity and recursions will exist
			((all s: cs | one nf: f.derived | nf.from = s and nf.to = jn.flow[Triggers])
			and (all nf: f.derived | nf.from in cs and nf.to = jn.flow[Triggers]))
	
	// The following 4 predicates flatten flows from states and the outermost start node to history nodes
	// When a history node is in a region and has no default flow (states + the outermost start state -> history nodes in regions and without a default flow)
	all f: Flows, hn: HistoryNodes | 
		(f.from in (States + StartNodes - (StartNodes & allContainedNodes)) 
		and f.to = hn and no hn.flow and hn in Regions.contains)
			implies let cs = (StartNodes & ((RegionsStates <: contains).contains.hn).contains.contains).flow[Triggers] |
				((all s: cs | one nf: f.derived | nf.from = f.from and nf.to = s)
				and (all nf: f.derived | nf.from = f.from and nf.to in cs))
	// When a history node is in a region and has a default flow (states + the outermost start state -> history nodes in regions and with a default flow)
	all f: Flows, hn: HistoryNodes | 
		(f.from in (States + StartNodes - (StartNodes & allContainedNodes)) 
		and f.to = hn and one hn.flow and hn in Regions.contains)
			implies let cs = hn.flow[Triggers] + nodesInOtherParallelRegions[contains.hn].flow[Triggers] |
				((all s: cs | one nf: f.derived | nf.from = f.from and nf.to = s)
				and (all nf: f.derived | nf.from = f.from and nf.to in cs))
	// When a history node is in a hierarchical state and has no default flow (states + the outermost start state -> history nodes in hierarchical states and without a default flow)
	all f: Flows, hn: HistoryNodes | 
		(f.from in (States + StartNodes - (StartNodes & allContainedNodes)) 
		and f.to = hn and no hn.flow and hn in HierarchicalStates.contains)
			implies let cs = (StartNodes & (HierarchicalStates <: contains).hn.contains).flow[Triggers] |
				((one nf: f.derived | nf.from = f.from and nf.to = cs)
				and (all nf: f.derived | nf.from = f.from and nf.to = cs))
	// When a history node is in a hierarchical state and has a default flow (states + the outermost start state -> history nodes in hierarchical states and with a default flow)
	all f: Flows, hn: HistoryNodes | 
		(f.from in (States + StartNodes - (StartNodes & allContainedNodes)) 
		and f.to = hn and one hn.flow and hn in HierarchicalStates.contains)
			implies ((one nf: f.derived | nf.from = f.from and nf.to = hn.flow[Triggers])
				and (all nf: f.derived | nf.from = f.from and nf.to = hn.flow[Triggers]))
	
	(Nodes - StartNodes - CompositeStates) in 
		(StartNodes - (StartNodes & allContainedNodes)).^((~from :> Flows) . (Flows <: to)) // Starting from the outermost start node, all nodes except composite states and start nodes can be reached. 
}

fact{
	trueReachability
}

run {} for 11 but exactly 1 JoinNodes, exactly 2 Regions
