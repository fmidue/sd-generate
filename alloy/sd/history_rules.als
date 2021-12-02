// About history nodes
module history_rules // most constraints of history nodes, but some constraints are directly with the signatures 

open components_sig as components // import all signatures

fact{
	// A history should be directed to a same or a deeper level, but definitely not to a level further outside
	all h1: History, c1: CompositeState |  #c1.inner = 0 && h1 in (c1.s_possess + c1.d_possess) => h1.flowto_triggerwith[Trigger] = none || h1.flowto_triggerwith[Trigger] in c1.^contains.*(inner.contains.(iden + ^contains))
	all h1: History, c1: CompositeState |  #c1.inner > 0 && h1 in (c1.inner.s_possess + c1.inner.d_possess) => h1.flowto_triggerwith[Trigger] = none || h1.flowto_triggerwith[Trigger] in c1.inner.contains.(iden + ^contains).*(inner.contains.(iden + ^contains))
	
	// History should never be reached from (somewhere, possibly nested) inside their own compound state
	all h1: History, c1: CompositeState | h1 in (c1.s_possess + c1.d_possess + c1.inner.s_possess + c1.inner.d_possess) 
							=> h1.flowfrom_triggerwith[Trigger] & (c1.^contains.*(inner.contains.(iden + ^contains)) +  c1.inner.contains.(iden + ^contains).*(inner.contains.(iden + ^contains)) - (StartState & c1.contains)) = none

	all h1: History, t1: Trigger | h1.flowto_triggerwith[t1] != none => t1.notated = none // Leaving transitions of history must be unconditional
	no h1: History | h1 not in (CompositeState.s_possess + CompositeState.d_possess + Region.s_possess + Region.d_possess) // No history nodes are at the outermost level of a state diagram
}
