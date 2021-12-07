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
one sig C1 extends CompositeState{}
one sig C2 extends CompositeState{}
one sig C3 extends CompositeState{}
one sig C4 extends CompositeState{}
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
//  6 kinds of triggers
one sig T0 extends Trigger{}
one sig T1 extends Trigger{}
one sig T2 extends Trigger{}
one sig T3 extends Trigger{}
one sig T4 extends Trigger{}
one sig T5 extends Trigger{}


fact{
	no (ForkNode + JoinNode + DeepHistory)
	T0.notated = none // Unconditional
	T1.notated = 0 // "OK"
	T2.notated = -1 // "+"
	T3.notated = -2 // "-"
	T4.notated = -3 // "L"
	T5.notated = -4 // "R"
	S1.flowto_triggerwith[T0] = C1
	C1.flowto_triggerwith[T1] = E1
	// Display screen
	C1.named = 5 
	C2.named = none
	C3.named = none
	C4.named = none
	C1.contains = (S2 + C2 + C3 + C4)
	S2.flowto_triggerwith[T0] = C2
	C2.flowto_triggerwith[T4] = H3
	C2.flowto_triggerwith[T5] = H2
	C3.flowto_triggerwith[T4] = H1
	C3.flowto_triggerwith[T5] = H3
	C4.flowto_triggerwith[T4] = H2
	C4.flowto_triggerwith[T5] = H1
	// In the leftmost digit
	N1.named = 1
	N2.named = 2
	N3.named = 3
	N4.named = 4
	N13.named = -5 
	R1.named = 6
	R2.named = 3
	C2.inner = R1 + R2
	R1.contains = S3 + N1 + N2 + N3 + N4 + H1
	R2.contains = S4 + N13
	S3.flowto_triggerwith[T0] = N1
	N1.flowto_triggerwith[T2] = N2
	N1.flowto_triggerwith[T3] = N4
	N2.flowto_triggerwith[T2] = N3
	N2.flowto_triggerwith[T3] = N1
	N3.flowto_triggerwith[T2] = N4
	N3.flowto_triggerwith[T3] = N2
	N4.flowto_triggerwith[T2] = N1
	N4.flowto_triggerwith[T3] = N3
	H1.flowto_triggerwith[T0] = N1
	S4.flowto_triggerwith[T0] = N13
	no N13.flowto_triggerwith
	// In the middle digit
	N5.named = 1
	N6.named = 2
	N7.named = 3
	N8.named = 4
	N14.named = -6
	R3.named = 6
	R4.named = 4
	C3.inner = R3 + R4
	R3.contains = S5 + N5 + N6 + N7 + N8 + H2
	R4.contains = S6 + N14
	S5.flowto_triggerwith[T0] = N5
	N5.flowto_triggerwith[T2] = N6
	N5.flowto_triggerwith[T3] = N8
	N6.flowto_triggerwith[T2] = N7
	N6.flowto_triggerwith[T3] = N5
	N7.flowto_triggerwith[T2] = N8
	N7.flowto_triggerwith[T3] = N6
	N8.flowto_triggerwith[T2] = N5
	N8.flowto_triggerwith[T3] = N7
	H2.flowto_triggerwith[T0] = N5
	S6.flowto_triggerwith[T0] = N14
	no N14.flowto_triggerwith
	// In the rightmost digit
	N9.named = 1
	N10.named = 2
	N11.named = 3
	N12.named = 4
	N15.named = -7
	R5.named = 6
	R6.named = 7
	C4.inner = R5 + R6
	R5.contains = S7+ N9 + N10 + N11 + N12 + H3
	R6.contains = S8 + N15
	S7.flowto_triggerwith[T0] = N9
	N9.flowto_triggerwith[T2] = N10
	N9.flowto_triggerwith[T3] = N12
	N10.flowto_triggerwith[T2] = N11
	N10.flowto_triggerwith[T3] = N9
	N11.flowto_triggerwith[T2] = N12
	N11.flowto_triggerwith[T3] = N10
	N12.flowto_triggerwith[T2] = N9
	N12.flowto_triggerwith[T3] = N11
	H3.flowto_triggerwith[T0] = N9
	S8.flowto_triggerwith[T0] = N15
	no N15.flowto_triggerwith
}

run {} for 10


