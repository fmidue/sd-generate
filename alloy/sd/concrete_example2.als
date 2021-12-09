module concrete_example2

open uml_state_diagram

// 1 start state
one sig S1 extends StartState{}
// 5 normal states
one sig N1 extends NormalState{}
one sig N2 extends NormalState{}
one sig N3 extends NormalState{}
one sig N4 extends NormalState{}
one sig N5 extends NormalState{}
// 2 composite states
one sig C1 extends RegionsState{}
one sig C2 extends RegionsState{}
// 4 Regions
one sig R1 extends Region{}
one sig R2 extends Region{}
one sig R3 extends Region{}
one sig R4 extends Region{}
// 2 fork nodes
one sig F1 extends ForkNode{}
one sig F2 extends ForkNode{}
// 2 join nodes
one sig J1 extends JoinNode{}
one sig J2 extends JoinNode{}
// 10 Names
one sig T0 extends EmptyName{}
one sig T1 extends NonEmptyName{}
one sig T2 extends NonEmptyName{}
one sig T3 extends NonEmptyName{}
one sig T4 extends NonEmptyName{}
one sig T5 extends NonEmptyName{}
one sig T6 extends NonEmptyName{}
one sig T7 extends NonEmptyName{}
one sig T8 extends NonEmptyName{}
one sig T9 extends NonEmptyName{}
// 1 end state
one sig E1 extends EndState{}

fact{
	no History
	N1.named = T3
	N2.named = T4
	N3.named = T5
	N4.named = T3
	N5.named = T4
	
	R1.named = T6
	R2.named = T7
	R3.named = T8
	R4.named = T9

	C1.inner = R1 + R2
	C2.inner = R3 + R4

	R1.r_contains = C2 + F2 + J1
	R2.r_contains = N3 + N5
	R3.r_contains = N1 + N2
	R4.r_contains = N4

	S1.flowto_triggerwith[T0] = F1
	F1.flowto_triggerwith[T0] = F2 + N3
	F2.flowto_triggerwith[T0] = N1 + N4
	N1.flowto_triggerwith[T1] = N2
	no N1.flowto_triggerwith[Name - T1]
	N2.flowto_triggerwith[T2] = J1
	no N2.flowto_triggerwith[Name - T2]
	N3.flowto_triggerwith[T1] = N5 
	no N3.flowto_triggerwith[Name - T1]
	N4.flowto_triggerwith[T2] = J1
	no N4.flowto_triggerwith[Name - T2]
	N5.flowto_triggerwith[T0] = J2
	no N5.flowto_triggerwith[Name - T0]
	J1.flowto_triggerwith[T0] = J2
	J2.flowto_triggerwith[T0] = E1
	no C1.flowto_triggerwith
	no C2.flowto_triggerwith	
}

run {} for 10
