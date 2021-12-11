module concrete_example2

open uml_state_diagram

// 1 start state
one sig S1 extends StartStates{}
// 5 normal states
one sig N1 extends NormalStates{}
one sig N2 extends NormalStates{}
one sig N3 extends NormalStates{}
one sig N4 extends NormalStates{}
one sig N5 extends NormalStates{}
// 2 composite states
one sig C1 extends RegionsStates{}
one sig C2 extends RegionsStates{}
// 4 Regionss
one sig R1 extends Regions{}
one sig R2 extends Regions{}
one sig R3 extends Regions{}
one sig R4 extends Regions{}
// 2 fork nodes
one sig F1 extends ForkNodes{}
one sig F2 extends ForkNodes{}
// 2 join nodes
one sig J1 extends JoinNodes{}
one sig J2 extends JoinNodes{}
// 2 names in the name space of triggers
one sig T1 extends TriggerNames{}
one sig T2 extends TriggerNames{}
// 7 name in the name space of states and regions
one sig Name1 extends ComponentNames{}
one sig Name2 extends ComponentNames{}
one sig Name3 extends ComponentNames{}
one sig Name4 extends ComponentNames{}
one sig Name5 extends ComponentNames{}
one sig Name6 extends ComponentNames{}
one sig Name7 extends ComponentNames{}
// 1 end state
one sig E1 extends EndStates{}

fact{
	no HistoryNodes
	N1.name = Name1
	N2.name = Name2
	N3.name = Name3
	N4.name = Name1
	N5.name = Name2
	
	R1.name = Name4
	R2.name = Name5
	R3.name = Name6
	R4.name = Name7

	C1.inner = R1 + R2
	C2.inner = R3 + R4

	R1.r_contains = C2 + F2 + J1
	R2.r_contains = N3 + N5
	R3.r_contains = N1 + N2
	R4.r_contains = N4

	S1.flow[EmptyTriggers] = F1
	F1.flow[EmptyTriggers] = F2 + N3
	F2.flow[EmptyTriggers] = N1 + N4
	N1.flow[T1] = N2
	no N1.flow[Triggers - T1]
	N2.flow[T2] = J1
	no N2.flow[Triggers - T2]
	N3.flow[T1] = N5 
	no N3.flow[Triggers - T1]
	N4.flow[T2] = J1
	no N4.flow[Triggers - T2]
	N5.flow[EmptyTriggers] = J2
	no N5.flow[Triggers - EmptyTriggers]
	J1.flow[EmptyTriggers] = J2
	J2.flow[EmptyTriggers] = E1
	no C1.flow
	no C2.flow	
}

run {} for 10
