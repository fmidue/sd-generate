module concrete_example2

open uml_state_diagram

// 1 start state
one sig S1 extends StartNodes{}
// 5 normal states
one sig N1 extends NormalStates{}
one sig N2 extends NormalStates{}
one sig N3 extends NormalStates{}
one sig N4 extends NormalStates{}
one sig N5 extends NormalStates{}
// 2 composite states
one sig C1 extends RegionsStates{}
one sig C2 extends RegionsStates{}
// 4 Regions
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
// 12 Flows
one sig Flow1 extends Flows{}
one sig Flow2 extends Flows{}
one sig Flow3 extends Flows{}
one sig Flow4 extends Flows{}
one sig Flow5 extends Flows{}
one sig Flow6 extends Flows{}
one sig Flow7 extends Flows{}
one sig Flow8 extends Flows{}
one sig Flow9 extends Flows{}
one sig Flow10 extends Flows{}
one sig Flow11 extends Flows{}
one sig Flow12 extends Flows{}
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
one sig E1 extends EndNodes{}

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

	C1.contains = R1 + R2
	C2.contains = R3 + R4

	R1.contains = C2 + F2 + J1
	R2.contains = N3 + N5
	R3.contains = N1 + N2
	R4.contains = N4

	Flows = Flow1 + Flow2 + Flow3 + Flow4 
		+ Flow5 + Flow6 + Flow7 + Flow8 
		+Flow9 + Flow10 + Flow11 + Flow12
	
	Flow1.from = S1
	Flow1.label = EmptyTrigger
	Flow1.to = F1
	
	Flow2.from = F1
	Flow2.label = EmptyTrigger
	Flow2.to = F2
	
	Flow3.from = F1
	Flow3.label = EmptyTrigger
	Flow3.to = N3

	Flow4.from = F2
	Flow4.label = EmptyTrigger
	Flow4.to = N1

	Flow5.from = F2
	Flow5.label = EmptyTrigger
	Flow5.to = N4

	Flow6.from = N1
	Flow6.label = T1
	Flow6.to = N2
	
	Flow7.from = N2
	Flow7.label = T2
	Flow7.to = J1

	Flow8.from = N3
	Flow8.label = T1
	Flow8.to = N5 
	
	Flow9.from = N4
	Flow9.label = T2
	Flow9.to = J1
	
	Flow10.from = N5
	Flow10.label = EmptyTrigger
	Flow10.to = J2

	Flow11.from = J1
	Flow11.label = EmptyTrigger
	Flow11.to = J2
	
	Flow12.from = J2
	Flow12.label = EmptyTrigger
	Flow12.to = E1
}

run {} for 12 but 21 ProtoFlows
