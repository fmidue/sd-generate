// About reachability
module reachability_rules // Most constraints of reachability, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	// Each normal/fork/join/history/end state has at least one incoming arrow (from a start state or somewhere else) 
	all n1: (Node - StartState - CompositeState) | n1 in (Node - n1).flowto_triggerwith[Trigger]

	// Each composite state has at least one entry
	all h1: HierarchicalState | let n1 = getAllNodeInSameAndDeeperLevel[h1], n2 = Node - h1 - n1 | h1 in n2.flowto_triggerwith[Trigger] || some (n1 & n2.flowto_triggerwith[Trigger]) // Standard, direct or history entry
	all rs1: RegionsState | let n1 = getAllNodeInSameAndDeeperLevel[rs1.inner], n2 = Node - rs1 - n1 | rs1 in n2.flowto_triggerwith[Trigger] //Standard entry
																		|| some (n1 & n2.flowto_triggerwith[Trigger]) // Direct or history entry
																		|| some f1: ForkNode | (f1 in n2.flowto_triggerwith[Trigger] && some (n1 & f1.flowto_triggerwith[Trigger])) // Fork entry
	// If there are history entries without default leaving transition, there must be a start state, because history nodes have neither record and a default leaving transitionat at the first entry
	all h1: HierarchicalState, h2: History | let n1 = h1.h_contains | no h2.flowto_triggerwith && h2 in n1 & (Node - h1 - n1).flowto_triggerwith[Trigger] => one (StartState & n1)
	all r1: Region, h1: History | let n1 = r1.r_contains | no h1.flowto_triggerwith && h1 in n1 & (Node - n1).flowto_triggerwith[Trigger] => one (StartState & n1)
	// If a composite state has regions and there are direct entries to one of the regions(except fork nodes), other regions must have start states
	all disj r1, r2: Region, rs1: RegionsState | let n1 = getAllNodeInSameAndDeeperLevel[r1] | (r1 + r2) in rs1.inner && some (n1 & (Node - rs1 - n1 - ForkNode).flowto_triggerwith[Trigger]) => one (StartState & r2.r_contains)
	// If a composite without regions has a standard entry, there must be a start state in it.
	all h1: HierarchicalState | h1 in Node.flowto_triggerwith[Trigger] => one (StartState & h1.h_contains)
	// If a composite with regions has a standard entry, there must be a start state in each region.
	all rs1: RegionsState, r1:Region | r1 in rs1.inner && rs1 in Node.flowto_triggerwith[Trigger] => one (StartState & r1.r_contains) 
}
