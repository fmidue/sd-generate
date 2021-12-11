module concrete_example1 // It demonstrates the example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/concretePositiveExample01.svg"

open uml_state_diagram // import all abstract signatures and constraints

// 8 start Statess
one sig S1 extends StartStates{}
one sig S2 extends StartStates{}
one sig S3 extends StartStates{}
one sig S4 extends StartStates{}
one sig S5 extends StartStates{}
one sig S6 extends StartStates{}
one sig S7 extends StartStates{}
one sig S8 extends StartStates{}
// 15 normal Statess
one sig N1 extends NormalStates{}
one sig N2 extends NormalStates{}
one sig N3 extends NormalStates{}
one sig N4 extends NormalStates{}
one sig N5 extends NormalStates{}
one sig N6 extends NormalStates{}
one sig N7 extends NormalStates{}
one sig N8 extends NormalStates{}
one sig N9 extends NormalStates{}
one sig N10 extends NormalStates{}
one sig N11 extends NormalStates{}
one sig N12 extends NormalStates{}
one sig N13 extends NormalStates{}
one sig N14 extends NormalStates{}
one sig N15 extends NormalStates{}
// 4 composite Statess
one sig C1 extends HierarchicalStates{}
one sig C2 extends RegionsStates{}
one sig C3 extends RegionsStates{}
one sig C4 extends RegionsStates{}
// 6 regions
one sig R1 extends Regions{}
one sig R2 extends Regions{}
one sig R3 extends Regions{}
one sig R4 extends Regions{}
one sig R5 extends Regions{}
one sig R6 extends Regions{}
// 3 shallow history
one sig H1 extends ShallowHistoryNodes{}
one sig H2 extends ShallowHistoryNodes{}
one sig H3 extends ShallowHistoryNodes{}
// 1 end States
one sig E1 extends EndStates{}
// 10 names in Names spaces of Statess and regions
one sig Name1 extends ComponentNames{}
one sig Name2 extends ComponentNames{}
one sig Name3 extends ComponentNames{}
one sig Name4 extends ComponentNames{}
one sig Name5 extends ComponentNames{}
one sig Name6 extends ComponentNames{}
one sig Name7 extends ComponentNames{}
one sig Name8 extends ComponentNames{}
one sig Name9 extends ComponentNames{}
one sig Name10 extends ComponentNames{}
// 5 names in Names spaces of triggers
one sig T1 extends TriggerNames{}
one sig T2 extends TriggerNames{}
one sig T3 extends TriggerNames{}
one sig T4 extends TriggerNames{}
one sig T5 extends TriggerNames{}

fact{
	no (ForkNodes + JoinNodes + DeepHistoryNodes)
//	Nodes.flow.Nodes in (EmptyName + T1 + T2 + T3 + T4 + T5)
	S1.flow[EmptyTriggers] = C1
	C1.flow[T1] = E1
	// Display screen
	C1.name = Name5 
	no C2.name
	no C3.name
	no C4.name
	C1.h_contains = (S2 + C2 + C3 + C4)
	S2.flow[EmptyTriggers] = C2
	C2.flow[T4] = H3
	C2.flow[T5] = H2
	C3.flow[T4] = H1
	C3.flow[T5] = H3
	C4.flow[T4] = H2
	C4.flow[T5] = H1
	// In the leftmost digit
	N1.name = Name1
	N2.name = Name2
	N3.name = Name3
	N4.name = Name4
	N13.name = Name8 
	R1.name = Name7
	R2.name = Name3
	C2.inner = R1 + R2
	R1.r_contains = S3 + N1 + N2 + N3 + N4 + H1
	R2.r_contains = S4 + N13
	S3.flow[EmptyTriggers] = N1
	N1.flow[T2] = N2
	N1.flow[T3] = N4
	N2.flow[T2] = N3
	N2.flow[T3] = N1
	N3.flow[T2] = N4
	N3.flow[T3] = N2
	N4.flow[T2] = N1
	N4.flow[T3] = N3
	H1.flow[EmptyTriggers] = N1
	S4.flow[EmptyTriggers] = N13
	no N13.flow
	// In the middle digit
	N5.name = Name1
	N6.name = Name2
	N7.name = Name3
	N8.name = Name4
	N14.name = Name9
	R3.name = Name6
	R4.name = Name4
	C3.inner = R3 + R4
	R3.r_contains = S5 + N5 + N6 + N7 + N8 + H2
	R4.r_contains = S6 + N14
	S5.flow[EmptyTriggers] = N5
	N5.flow[T2] = N6
	N5.flow[T3] = N8
	N6.flow[T2] = N7
	N6.flow[T3] = N5
	N7.flow[T2] = N8
	N7.flow[T3] = N6
	N8.flow[T2] = N5
	N8.flow[T3] = N7
	H2.flow[EmptyTriggers] = N5
	S6.flow[EmptyTriggers] = N14
	no N14.flow
	// In the rightmost digit
	N9.name = Name1
	N10.name = Name2
	N11.name = Name3
	N12.name = Name4
	N15.name = Name10
	R5.name = Name6
	R6.name = Name7
	C4.inner = R5 + R6
	R5.r_contains = S7+ N9 + N10 + N11 + N12 + H3
	R6.r_contains = S8 + N15
	S7.flow[EmptyTriggers] = N9
	N9.flow[T2] = N10
	N9.flow[T3] = N12
	N10.flow[T2] = N11
	N10.flow[T3] = N9
	N11.flow[T2] = N12
	N11.flow[T3] = N10
	N12.flow[T2] = N9
	N12.flow[T3] = N11
	H3.flow[EmptyTriggers] = N9
	S8.flow[EmptyTriggers] = N15
	no N15.flow
}

run {} for 10


