open uml_state_diagram

abstract sig Flows{
	from: some (Nodes - EndStates),
	to: some (Nodes - StartStates)
}

abstract sig OriginalFlows extends Flows{
	derive: disj lone TranslatedFlows
}
{
	one from
	one to
}

abstract sig TranslatedFlows extends Flows{}
{
	no ((CompositeStates + HistoryNodes) & from)
	no ((CompositeStates + HistoryNodes) & to)
}

fun nodesInOtherParallelRegions[rs: set Regions]: set Nodes{
	((contains.rs).(RegionsStates <: contains) - rs).contains
}

fun nodesInOtherParallelRegionsAndDeeper[rs: set Regions]: set Nodes{
	nodesInThisAndDeeper[contains.rs] - nodesInThisAndDeeper[rs]
}

pred trueReachability{
	atLeastOneEntryToCompositeStates
	no disj f1, f2: Flows | f1.from = f2.from && f1.to = f2.to
	all n1, n2: Nodes | n2 in n1.flow[Triggers] 
		implies (one of1: OriginalFlows | of1.from = n1 and of1.to = n2) 
		else (no of2: OriginalFlows | of2.from = n1 and of2.to = n2)
	all tf1: TranslatedFlows | tf1 in OriginalFlows.derive
	// Type 1
	all n1, n2: Nodes - HistoryNodes - CompositeStates, of1: OriginalFlows | 
		of1.from = n1 and of1.to = n2 and (n1 + n2) not in Regions.contains
			implies no of1.derive
	// Type 3, 7, 9
	all disj n1, n2: Nodes - HistoryNodes - CompositeStates, of1: OriginalFlows | 
		let tf1 = of1.derive, r1 = (Regions <: contains).n1, r2 = (Regions <: contains).n2 |
		{ 
			(n1 in Regions.contains and n2 not in Regions.contains and of1.from = n1 and of1.to = n2
				implies one tf1 and tf1.to = n2
					and tf1.from = n1 + nodesInOtherParallelRegionsAndDeeper[r1]) // Type 7
			(n1 in Regions.contains and n2 not in Regions.contains and of1.from = n2 and of1.to = n1
				implies one tf1 and tf1.from = n2
					and tf1.to = n2 + nodesInOtherParallelRegions[r2] 
						+ ((nodesInOtherParallelRegions[r2] & CompositeStates).contains & StartStates).flow[Triggers]) // Type 3
			((n1 + n2) in Regions.contains and n2 not in Regions.contains and of1.from = n2 and of1.to = n1
				implies one tf1 
					and tf1.from = n1 + nodesInOtherParallelRegionsAndDeeper[r1]
					and tf1.to = n2 + nodesInOtherParallelRegions[r2]
						+ ((nodesInOtherParallelRegions[r2] & CompositeStates).contains & StartStates).flow[Triggers]) // Type 9
		}
	// Type 2, 4, 6, 8
	all n1: Nodes - HistoryNodes - CompositeStates, c1: CompositeStates, of1: OriginalFlows | 
		let tf1 = of1.derive, r1 = (Regions <: contains).n1, r2 = (Regions <: contains).c1 | 
		{	
			(of1.from = c1 and of1.to = n1 and (n1 + c1) not in Regions.contains
					implies one tf1 and tf1.from = nodesInThisAndDeeper[c1] and tf1.to = n1) // Type 4
			(of1.from = n1 and of1.to = c1 and (n1 + c1) not in Regions.contains
				implies one tf1 and tf1.from = n1 and tf1.to = (StartStates & c1.contains).flow[Triggers]) // Type 2
			(of1.from = c1 and of1.to = n1 and n1 in Regions.contains and c1 not in Regions.contains
				implies one tf1 and tf1.from = nodesInThisAndDeeper[c1] 
					and tf1.to = n1 + nodesInOtherParallelRegions[r1]
						+ ((nodesInOtherParallelRegions[r1] & CompositeStates).contains & StartStates).flow[Triggers]) // Type 6
			(of1.from = n1 and of1.to = c1 and n1 in Regions.contains and c1 not in Regions.contains
				implies one tf1 and tf1.from = n1 + nodesInOtherParallelRegionsAndDeeper[r1] 
					and tf1.to = (c1.contains & StartStates).flow[Triggers]) // Type 8
			(of1.from = n1 and of1.to = c1 and n1 not in Regions.contains and c1 in Regions.contains
				implies one tf1 and tf1.from = n1
					and tf1.to = (c1.contains & StartStates).flow[Triggers] + nodesInOtherParallelRegions[r2]
						+ ((nodesInOtherParallelRegions[r2] & CompositeStates).contains & StartStates).flow[Triggers])
			(of1.from = c1 and of1.to = n1 and n1 not in Regions.contains and c1 in Regions.contains
				implies one tf1 and tf1.to = n1
					and tf1.from = nodesInThisAndDeeper[c1] + nodesInOtherParallelRegionsAndDeeper[r1])
			(of1.from = n1 and of1.to = c1 and (n1 + c1) in Regions.contains
				implies one tf1 and tf1.from = n1 + nodesInOtherParallelRegionsAndDeeper[r1] 
					and tf1.to = (c1.contains & StartStates).flow[Triggers] + nodesInOtherParallelRegions[r2]
						+ ((nodesInOtherParallelRegions[r2] & CompositeStates).contains & StartStates).flow[Triggers])
			(of1.from = c1 and of1.to = n1 and (n1 + c1) in Regions.contains
				implies one tf1 and tf1.from = nodesInThisAndDeeper[c1] + nodesInOtherParallelRegionsAndDeeper[r1]
					and tf1.to = n1 + nodesInOtherParallelRegions[r1]
						+ ((nodesInOtherParallelRegions[r1] & CompositeStates).contains & StartStates).flow[Triggers])
		}
	// Type 5 
/*	all c1, c2: CompositeStates, of1: OriginalFlows |
		(of1.from = c1 and of1.to = c2 and n1 not in Regions.contains
				implies one tf1 and tf1.from = nodesInThisAndDeeper[c1] and tf1.to = n1)*/
//	all n1: Nodes, h1:ShallowHistoryNodes, of1: OriginalFlow | let tf1 = of1.derive |
	(Nodes - StartStates - CompositeStates) in (StartStates - allContainedNodes).^((~from :> Flows) . (Flows <: to))
}

fact{
	trueReachability
}

run {} for 15 but exactly 2 HierarchicalStates, exactly 8 NormalStates
