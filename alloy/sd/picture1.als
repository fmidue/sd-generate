module picture1 // It is correspoding to the Haskell version "picture1"

open uml_state_diagram // "UMLStateDiagram"

// 2 composite states
one sig C1 extends HierarchicalStates{} // "Composite State"
one sig C2 extends HierarchicalStates{} // "state 2"

// 4 normal states
one sig N1 extends NormalStates{} // "State 1"
one sig N2 extends NormalStates{} // "State 2a"
one sig N3 extends NormalStates{} // "State 2b"
one sig N4 extends NormalStates{} // "State 3"

// 2 start states
one sig S1 extends StartStates{} // The start state in "Composite State"
one sig S2 extends StartStates{} // The start state in "state 2"

// 6 component names
one sig Name1 extends ComponentNames{} // "Composite State"
one sig Name2 extends ComponentNames{} // "state 2"
one sig Name3 extends ComponentNames{} // "State 1"
one sig Name4 extends ComponentNames{} // "State 2a"
one sig Name5 extends ComponentNames{} // "State 2b"
one sig Name6 extends ComponentNames{} // "State 3"

// 1 TriggerNames
one sig T1 extends TriggerNames{} // "t"

// 1 DeepHistoryNodes
one sig H1 extends DeepHistoryNodes{} // "H*" in "Composite State"

fact{
	//Outermost level
	/* 
	  picture1 = StateDiagram [a, b] 1 "" [Connection[1] [2] "t", Connection[2] [1,3] ""] []  is the outermost level
	  a and b in the outermost level; 
	  "1" is a string as a label; 
	  "" means no name; 
	  Connection[1] [2] "t" means what is labeled with "1" has connection "t" to what is labeled with "2"; 
	  Connection[2] [1,3] "" means what is labeled with "2" has connection "" to what is labeled with "3" in what is labeled with "1"
	  [] means no start states, if it has elements, the integer is the label of start states and the number of integer is the number of start states 
	*/
	// under a same "where" like "a" = C1, ‘b“ = N4 in a same level(outermost level) 
	C1.flow[T1] = N4 // According to the label, we can replace Connection[1] [2] "t" by Connection a b "t", it can be transformed into a.flow[t] = b, namely C1.flow[T1] = N4
	no C1.flow[Triggers - T1] // If a node has flow[EmptyTrigger], adding this constraint is optional, but if it has non-empty labeled flow, it is necessary. 
	C1.name = Name1 // "Composite State"
	N4.name = Name6 // "State 3"
	
	// In the composte state "Composite State"
	/*
	  a = StateDiagram [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
	  The translation like above.
	  It is noticeable that S1 is labeled with "1"
	*/
	//under a same "where" like “c” = N1, ‘d“ = C2, "e" = H1 in a same level("Composite State")
	C1.contains = S1 + N1 + C2 + H1 // According to a = StateDiagram [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1], we can get "c","d","e" and a start state labeled with "1" in it
	S1.flow[EmptyTrigger] = N1 // a: Connection [1] [2] "" => 1.flow[""] = 2 => S1.flow[EmptyTrigger] = N1
	N1.flow[EmptyTrigger] = C2 // a: Connection [1] [2] "" => 1.flow[""] = 2 => c.flow[""] = d => N1.flow[EmptyTrigger] = C2, too 
	N4.flow[EmptyTrigger] = H1 // picture1: Connection[2] [1,3] "" => 2.flow[""] = 1.3 => b.flow[""] = e => N4.flow[EmptyTrigger] = H1
	no C2.flow // If a node has no flow, this is necessary
	no H1.flow
	C2.name = Name2 // "state 2"
	N1.name = Name3 // "State 1"

	//In the composte state "state 2"
	/*
	  d = StateDiagram [f,g] 2 "state 2" [Connection [1] [2] ""] [1]
	  The translation like above.
	  It is noticeable that S2 is labeled with "1"
	*/
	// under a same "where" like “f” = N4, ‘g“ = N5, in a same level("Composite State")
	C2.contains = S2 + N2 + N3 // According to d = StateDiagram [f,g] 2 "state 2" [Connection [1] [2] ""] [1], we can get "f","g" and a start state labeled with "1" in it
	S2.flow[EmptyTrigger] = N2 // d: Connection [1] [2] "" => 1.flow[""] = 2 => S1.flow[EmptyTrigger] = N4
	N2.flow[EmptyTrigger] = N3 // d: Connection [1] [2] "" => 1.flow[""] = 2 => f.flow[""] = g => N4.flow[EmptyTrigger] = N5, too
	no N3.flow
	N2.name = Name4 // "State 2a"
	N3.name = Name5 // "State 2b"
}

run {} for 6
