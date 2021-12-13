module concrete_example1 // It demonstrates the example "https://github.com/fmidue/ba-zixin-wu/blob/master/examples/concretePositiveExample01.svg"

open uml_state_diagram // import all abstract signatures and constraints

// 8 start Statess
one sig S1 extends StartStates{} // S1 is the outermost start state
one sig S2 extends StartStates{} // S2 is tte start state in "Display screen"
one sig S3 extends StartStates{} // S3 is the start state in the first region in "The leftmost digit"
one sig S4 extends StartStates{} // S4 is the start state in the second region in "The leftmost digit"
one sig S5 extends StartStates{} // S5 is the start state in the first region in "The middle digit"
one sig S6 extends StartStates{} // S6 is the start state in the second region in "The middle digit"
one sig S7 extends StartStates{} // S7 is the start state in the first region in "The rightmost digit"
one sig S8 extends StartStates{} // S8 is the start state in the second region in "The rightmost digit"
// 15 normal Statess
one sig N1 extends NormalStates{} // “1” in "The leftmost digit"
one sig N2 extends NormalStates{} // "2" in "The leftmost digit"
one sig N3 extends NormalStates{} // "3" in "The leftmost digit"
one sig N4 extends NormalStates{} // "4" in "The leftmost digit"
one sig N5 extends NormalStates{} // "1" in "The middle digit"
one sig N6 extends NormalStates{} // "2" in "The middle digit"
one sig N7 extends NormalStates{} // "3" in "The middle digit"
one sig N8 extends NormalStates{} // "4" in "The middle digit"
one sig N9 extends NormalStates{} // "1" in "The rightmost digit"
one sig N10 extends NormalStates{} // "2" in "The rightmost digit"
one sig N11 extends NormalStates{} // "3" in "The rightmost digit"
one sig N12 extends NormalStates{} // "4" in "The rightmost digit"
one sig N13 extends NormalStates{} // “leftmost” in "The leftmost digit"
one sig N14 extends NormalStates{} // "middle" in "The middle digit"
one sig N15 extends NormalStates{} // "rightmost" in "The rightmost digit"
// 4 composite Statess
one sig C1 extends HierarchicalStates{} // The outermost hierarchical state
one sig C2 extends RegionsStates{} //The leftmost regions state
one sig C3 extends RegionsStates{} // The middle regions state
one sig C4 extends RegionsStates{} // The rightmost regions state
// 6 regions
one sig R1 extends Regions{} // The first region in the leftmost regions state
one sig R2 extends Regions{} // The second region in the leftmost regions state
one sig R3 extends Regions{} // The first region in the middle regions state
one sig R4 extends Regions{} // The secnod region in the middle regions state
one sig R5 extends Regions{} // The first region in the rightmoste regions state
one sig R6 extends Regions{} // The second region in the rightmoste regions state
// 3 shallow history
one sig H1 extends ShallowHistoryNodes{} // The history node in the leftmost regions state
one sig H2 extends ShallowHistoryNodes{} // The history node in the middle regions state
one sig H3 extends ShallowHistoryNodes{} // The history node in the rightmoste regions state
// 1 end States
one sig E1 extends EndStates{} // The outermost end state
// 10 names in Names spaces of Statess and regions
one sig Name1 extends ComponentNames{} //”1“
one sig Name2 extends ComponentNames{} //”2“
one sig Name3 extends ComponentNames{} //”3“
one sig Name4 extends ComponentNames{} //”4“
one sig Name5 extends ComponentNames{} //”Display screen"
one sig Name6 extends ComponentNames{} //"The leftmost digit"
one sig Name7 extends ComponentNames{} //"The middle digit"
one sig Name8 extends ComponentNames{} //"The rightmost digit"
one sig Name9 extends ComponentNames{} //"leftmost"
one sig Name10 extends ComponentNames{} //"middle"
one sig Name11 extends ComponentNames{} //"rightmost"
// 5 names in Names spaces of triggers
one sig T1 extends TriggerNames{} // "OK"
one sig T2 extends TriggerNames{} // “+”
one sig T3 extends TriggerNames{} // ”-“
one sig T4 extends TriggerNames{} // “L“
one sig T5 extends TriggerNames{} // “R”

fact{
	no (ForkNodes + JoinNodes + DeepHistoryNodes)
	S1.flow[EmptyTrigger] = C1
	C1.flow[T1] = E1
	// Display screen
	C1.name = Name5 //"Display screen"
	no C2.name
	no C3.name
	no C4.name
	C1.contains = (S2 + C2 + C3 + C4)
	S2.flow[EmptyTrigger] = C2
	C2.flow[T4] = H3
	C2.flow[T5] = H2
	C3.flow[T4] = H1
	C3.flow[T5] = H3
	C4.flow[T4] = H2
	C4.flow[T5] = H1
	// In the leftmost digit
	N1.name = Name1 //”1“
	N2.name = Name2 //”2“
	N3.name = Name3 //”3“
	N4.name = Name4 //”4“
	N13.name = Name9 //"leftmost"
	R1.name = Name6 //"The leftmost digit"
	no R2.name
	C2.contains = R1 + R2
	R1.contains = S3 + N1 + N2 + N3 + N4 + H1
	R2.contains = S4 + N13
	S3.flow[EmptyTrigger] = N1
	N1.flow[T2] = N2
	N1.flow[T3] = N4
	N2.flow[T2] = N3
	N2.flow[T3] = N1
	N3.flow[T2] = N4
	N3.flow[T3] = N2
	N4.flow[T2] = N1
	N4.flow[T3] = N3
	H1.flow[EmptyTrigger] = N1
	S4.flow[EmptyTrigger] = N13
	no N13.flow
	// In the middle digit
	N5.name = Name1 //”1“
	N6.name = Name2 //”2“
	N7.name = Name3 //”3“ 
	N8.name = Name4 //”4“
	N14.name = Name10 //"middle"
	R3.name = Name7 //"The middle digit"
	no R4.name
	C3.contains = R3 + R4
	R3.contains = S5 + N5 + N6 + N7 + N8 + H2
	R4.contains = S6 + N14
	S5.flow[EmptyTrigger] = N5
	N5.flow[T2] = N6
	N5.flow[T3] = N8
	N6.flow[T2] = N7
	N6.flow[T3] = N5
	N7.flow[T2] = N8
	N7.flow[T3] = N6
	N8.flow[T2] = N5
	N8.flow[T3] = N7
	H2.flow[EmptyTrigger] = N5
	S6.flow[EmptyTrigger] = N14
	no N14.flow
	// In the rightmost digit
	N9.name = Name1 //”1“
	N10.name = Name2 //”2“
	N11.name = Name3 //”3“ 
	N12.name = Name4 //”4“
	N15.name = Name11 //"rightmost"
	R5.name = Name8 //"The rightmost digit"
	no R6.name
	C4.contains = R5 + R6
	R5.contains = S7+ N9 + N10 + N11 + N12 + H3
	R6.contains = S8 + N15
	S7.flow[EmptyTrigger] = N9
	N9.flow[T2] = N10
	N9.flow[T3] = N12
	N10.flow[T2] = N11
	N10.flow[T3] = N9
	N11.flow[T2] = N12
	N11.flow[T3] = N10
	N12.flow[T2] = N9
	N12.flow[T3] = N11
	H3.flow[EmptyTrigger] = N9
	S8.flow[EmptyTrigger] = N15
	no N15.flow
}

run {} for 10


