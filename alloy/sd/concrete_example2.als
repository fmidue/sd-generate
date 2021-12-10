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
// 2 names in the name space of triggers
one sig T1 extends NonEmptyTrigger{}
one sig T2 extends NonEmptyTrigger{}
// 7 name in the name space of states and regions
one sig Name1 extends NonEmptyName{}
one sig Name2 extends NonEmptyName{}
one sig Name3 extends NonEmptyName{}
one sig Name4 extends NonEmptyName{}
one sig Name5 extends NonEmptyName{}
one sig Name6 extends NonEmptyName{}
one sig Name7 extends NonEmptyName{}
// 1 end state
one sig E1 extends EndState{}

fact{
	no History
	N1.named = Name1
	N2.named = Name2
	N3.named = Name3
	N4.named = Name1
	N5.named = Name2
	
	R1.named = Name4
	R2.named = Name5
	R3.named = Name6
	R4.named = Name7

	C1.inner = R1 + R2
	C2.inner = R3 + R4

	R1.r_contains = C2 + F2 + J1
	R2.r_contains = N3 + N5
	R3.r_contains = N1 + N2
	R4.r_contains = N4

	S1.flowto_triggerwith[EmptyTrigger] = F1
	F1.flowto_triggerwith[EmptyTrigger] = F2 + N3
	F2.flowto_triggerwith[EmptyTrigger] = N1 + N4
	N1.flowto_triggerwith[T1] = N2
	no N1.flowto_triggerwith[Name - T1]
	N2.flowto_triggerwith[T2] = J1
	no N2.flowto_triggerwith[Name - T2]
	N3.flowto_triggerwith[T1] = N5 
	no N3.flowto_triggerwith[Name - T1]
	N4.flowto_triggerwith[T2] = J1
	no N4.flowto_triggerwith[Name - T2]
	N5.flowto_triggerwith[EmptyTrigger] = J2
	no N5.flowto_triggerwith[Name - EmptyTrigger]
	J1.flowto_triggerwith[EmptyTrigger] = J2
	J2.flowto_triggerwith[EmptyTrigger] = E1
	no C1.flowto_triggerwith
	no C2.flowto_triggerwith	
}

run {} for 10
