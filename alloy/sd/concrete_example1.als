module concrete_example1 // It demonstrates the example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/concretePositiveExample01.svg"

open uml_state_diagram // import all abstract signatures and constraints

// 8 start states
one sig S1 extends StartState{}
one sig S2 extends StartState{}
one sig S3 extends StartState{}
one sig S4 extends StartState{}
one sig S5 extends StartState{}
one sig S6 extends StartState{}
one sig S7 extends StartState{}
one sig S8 extends StartState{}
// 15 normal states
one sig N1 extends NormalState{}
one sig N2 extends NormalState{}
one sig N3 extends NormalState{}
one sig N4 extends NormalState{}
one sig N5 extends NormalState{}
one sig N6 extends NormalState{}
one sig N7 extends NormalState{}
one sig N8 extends NormalState{}
one sig N9 extends NormalState{}
one sig N10 extends NormalState{}
one sig N11 extends NormalState{}
one sig N12 extends NormalState{}
one sig N13 extends NormalState{}
one sig N14 extends NormalState{}
one sig N15 extends NormalState{}
// 4 composite states
one sig C1 extends HierarchicalState{}
one sig C2 extends RegionsState{}
one sig C3 extends RegionsState{}
one sig C4 extends RegionsState{}
// 6 regions
one sig R1 extends Region{}
one sig R2 extends Region{}
one sig R3 extends Region{}
one sig R4 extends Region{}
one sig R5 extends Region{}
one sig R6 extends Region{}
// 3 shallow history
one sig H1 extends ShallowHistory{}
one sig H2 extends ShallowHistory{}
one sig H3 extends ShallowHistory{}
// 1 end state
one sig E1 extends EndState{}
// 10 names in name spaces of states and regions
one sig Name1 extends NonEmptyName{}
one sig Name2 extends NonEmptyName{}
one sig Name3 extends NonEmptyName{}
one sig Name4 extends NonEmptyName{}
one sig Name5 extends NonEmptyName{}
one sig Name6 extends NonEmptyName{}
one sig Name7 extends NonEmptyName{}
one sig Name8 extends NonEmptyName{}
one sig Name9 extends NonEmptyName{}
one sig Name10 extends NonEmptyName{}
// 5 names in name spaces of triggers
one sig T1 extends NonEmptyTrigger{}
one sig T2 extends NonEmptyTrigger{}
one sig T3 extends NonEmptyTrigger{}
one sig T4 extends NonEmptyTrigger{}
one sig T5 extends NonEmptyTrigger{}

fact{
	no (ForkNode + JoinNode + DeepHistory)
//	Node.flowto_triggerwith.Node in (EmptyName + T1 + T2 + T3 + T4 + T5)
	S1.flowto_triggerwith[EmptyTrigger] = C1
	C1.flowto_triggerwith[T1] = E1
	// Display screen
	C1.named = Name5 
	no C2.named
	no C3.named
	no C4.named
	C1.h_contains = (S2 + C2 + C3 + C4)
	S2.flowto_triggerwith[EmptyTrigger] = C2
	C2.flowto_triggerwith[T4] = H3
	C2.flowto_triggerwith[T5] = H2
	C3.flowto_triggerwith[T4] = H1
	C3.flowto_triggerwith[T5] = H3
	C4.flowto_triggerwith[T4] = H2
	C4.flowto_triggerwith[T5] = H1
	// In the leftmost digit
	N1.named = Name1
	N2.named = Name2
	N3.named = Name3
	N4.named = Name4
	N13.named = Name8 
	R1.named = Name7
	R2.named = Name3
	C2.inner = R1 + R2
	R1.r_contains = S3 + N1 + N2 + N3 + N4 + H1
	R2.r_contains = S4 + N13
	S3.flowto_triggerwith[EmptyTrigger] = N1
	N1.flowto_triggerwith[T2] = N2
	N1.flowto_triggerwith[T3] = N4
	N2.flowto_triggerwith[T2] = N3
	N2.flowto_triggerwith[T3] = N1
	N3.flowto_triggerwith[T2] = N4
	N3.flowto_triggerwith[T3] = N2
	N4.flowto_triggerwith[T2] = N1
	N4.flowto_triggerwith[T3] = N3
	H1.flowto_triggerwith[EmptyTrigger] = N1
	S4.flowto_triggerwith[EmptyTrigger] = N13
	no N13.flowto_triggerwith
	// In the middle digit
	N5.named = Name1
	N6.named = Name2
	N7.named = Name3
	N8.named = Name4
	N14.named = Name9
	R3.named = Name6
	R4.named = Name4
	C3.inner = R3 + R4
	R3.r_contains = S5 + N5 + N6 + N7 + N8 + H2
	R4.r_contains = S6 + N14
	S5.flowto_triggerwith[EmptyTrigger] = N5
	N5.flowto_triggerwith[T2] = N6
	N5.flowto_triggerwith[T3] = N8
	N6.flowto_triggerwith[T2] = N7
	N6.flowto_triggerwith[T3] = N5
	N7.flowto_triggerwith[T2] = N8
	N7.flowto_triggerwith[T3] = N6
	N8.flowto_triggerwith[T2] = N5
	N8.flowto_triggerwith[T3] = N7
	H2.flowto_triggerwith[EmptyTrigger] = N5
	S6.flowto_triggerwith[EmptyTrigger] = N14
	no N14.flowto_triggerwith
	// In the rightmost digit
	N9.named = Name1
	N10.named = Name2
	N11.named = Name3
	N12.named = Name4
	N15.named = Name10
	R5.named = Name6
	R6.named = Name7
	C4.inner = R5 + R6
	R5.r_contains = S7+ N9 + N10 + N11 + N12 + H3
	R6.r_contains = S8 + N15
	S7.flowto_triggerwith[EmptyTrigger] = N9
	N9.flowto_triggerwith[T2] = N10
	N9.flowto_triggerwith[T3] = N12
	N10.flowto_triggerwith[T2] = N11
	N10.flowto_triggerwith[T3] = N9
	N11.flowto_triggerwith[T2] = N12
	N11.flowto_triggerwith[T3] = N10
	N12.flowto_triggerwith[T2] = N9
	N12.flowto_triggerwith[T3] = N11
	H3.flowto_triggerwith[EmptyTrigger] = N9
	S8.flowto_triggerwith[EmptyTrigger] = N15
	no N15.flowto_triggerwith
}

run {} for 10


