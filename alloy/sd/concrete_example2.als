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
one sig C1 extends CompositeState{}
one sig C2 extends CompositeState{}
// 4 Regions
one sig r1 extends Region{}
one sig r2 extends Region{}
one sig r3 extends Region{}
one sig r4 extends Region{}
// 2 fork nodes
one sig F1 extends ForkNode{}
one sig F2 extends ForkNode{}
// 2 join nodes
one sig J1 extends JoinNode{}
one sig J2 extends JoinNode{}
// 4 triggers
one sig T0 extends Trigger{}
one sig T1 extends Trigger{}
one sig T2 extends Trigger{}
one sig T3 extends Trigger{}
// 1 end state
one sig E1 extends EndState{}

fact{
	no History
	StartState
}

