module picture1 // It is correspoding to the Haskell version "picture1"

open uml_state_diagram // "UMLStateDiagram"

/* 
picture1 :: UMLStateDiagram
picture1 = StateDiagram [a,b] 1 "" [Connection[1] [2] "t", Connection[2] [1,3] ""] [1] // Here adds a start node in the outermost level
	where
		a = StateDiagram [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
			where
				c = InnerMostState 1 "State 1" ""
				d = StateDiagram [f,g] 2 "state 2" [Connection [1] [2] ""] [1]
				where
					f = InnerMostState 1 "State 2a" ""
					g = InnerMostState 2 "State 2b" ""
				e = History 3 Deep
		b = InnerMostState 2 "State 3" ""
*/

// 2 composite states
one sig C_1 extends HierarchicalStates{} // "Composite State", the suffix number can be regarded as address
one sig C_1_2 extends HierarchicalStates{} // "state 2"

// 4 normal states
one sig N_1_1 extends NormalStates{} // "State 1"
one sig N_1_2_1 extends NormalStates{} // "State 2a"
one sig N_1_2_2 extends NormalStates{} // "State 2b"
one sig N_2 extends NormalStates{} // "State 3"

// 2 start states
one sig S extends StartNodes{} // The start node in the outermost level has no suffix
one sig S_1 extends StartNodes{} // The start node in "Composite State", the suffix number signs that the start state is in which composite state.
one sig S_1_2 extends StartNodes{} // The start node in "state 2"

// 7 Flows
one sig SFlow extends Flows{} // The flow belongs to the start node in the outermost level.
one sig S_1Flow extends Flows{} // The flow belongs to the start node in "Composite State".
one sig S_1_2Flow extends Flows{} // The flow belongs to the start node in "state 2".
one sig Connection1 extends Flows{} // This "Connection" is corresponding to the "Connection" in the Haskell version and the suffix number is in sequency as reading the Haskell version
one sig Connection2 extends Flows{}
one sig Connection3 extends Flows{}
one sig Connection4 extends Flows{}

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
one sig H_1_3 extends DeepHistoryNodes{} // "H*" in "Composite State"

fact{
	no EndNodes
	Flows = SFlow + S_1Flow + S_1_2Flow + Connection1 + Connection2 + Connection3 + Connection4
	
	//Outermost level
	/* 
	  picture1 = StateDiagram [a, b] 1 "" [Connection[1] [2] "t", Connection[2] [1,3] ""] [1]  is the outermost level
	  a and b in the outermost level; 
	  "1" is a string as a label, also can be regarded as address. In Alloy model, it is added as a suffix "_1" of a signature name
	  "" means no name; regarding the outermost level, the label and the name can be discard directly.
	  Connection[1] [2] "t" means what is labeled with "_1" has connection "t" to what is labeled with "_2"; 
	  Connection[2] [1,3] "" means what is labeled with "_2" has connection "" to what is labeled with "_1_3"
	  [] means no start nodes, if it has elements, it means there is a start node directed to the node inside "picture1" and addressed with the elements
	  Here [1] means there is a start node in outermost level and directed to a suffix address is "_1" (because the label of outermost level is discarded)
	*/
	// under a same "where" like "a" = C_1, ‘b“ = N_2 in a same level(outermost level) 
	// picture1: [1] => SFlow from S to C_1 with EmptyTrigger
	SFlow.from = S
	SFlow.label = EmptyTrigger
	SFlow.to = C_1
	// According to the label, we can replace Connection[1] [2] "t" by Connection a b "t", it can be transformed into Connection1 from a to b with label "t"
	Connection1.from = C_1
	Connection1.label =T1
	Connection1.to = N_2

	C_1.name = Name1 // "Composite State"
	N_2.name = Name6 // "State 3"
	
	// In the composte state "Composite State"
	/*
	  a = StateDiagram [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1]
	  The translation like above.
	  It is noticeable that S_1 is directed to a node inside "a" (_1) and addressed with "_1" which can be translated into a node addressed with "_1_1"
	*/
	//under a same "where" like “c” = N_1_1, ‘d“ = C_1_2, "e" = H_1_3 in a same level("Composite State")
	C_1.contains = S_1 + N_1_1 + C_1_2 + H_1_3 // According to a = StateDiagram [c,d,e] 1 "Composite State" [Connection [1] [2] ""] [1], we can get "c","d","e" and a start state in it
	// a: [1] => S_1Flow from S_1 to N_1_1 with EmptyTrigger
	S_1Flow.from = S_1
	S_1Flow.label = EmptyTrigger
	S_1Flow.to = N_1_1
	// a: Connection [1] [2] "" => 1.flow[""] = 2 => c.flow[""] = d => Connection3 from N_1_1 to C_1_2 with EmptyTrigger
	Connection3.from = N_1_1
	Connection3.label = EmptyTrigger
	Connection3.to = C_1_2
	// picture1: Connection[2] [1,3] "" => 2.flow[""] = 1.3 => b.flow[""] = e => Connection2 from N_2 to H_1_3 with EmptyTrigger
	Connection2.from = N_2
	Connection2.label = EmptyTrigger
	Connection2.to = H_1_3 
	
	C_1_2.name = Name2 // "state 2"
	N_1_1.name = Name3 // "State 1"

	//In the composte state "state 2"
	/*
	  d = StateDiagram [f,g] 2 "state 2" [Connection [1] [2] ""] [1]
	  The translation like above.
	  It is noticeable that S_1_2 is directed to a node inside "d" (_1_2) and addressed with "_1" which can be translated into a node addressed with "_1_2_1"
	*/
	// under a same "where" like “f” = N_1_2_1, ‘g“ = N_1_2_2, in a same level("Composite State")
	C_1_2.contains = S_1_2 + N_1_2_1 + N_1_2_2 // According to d = StateDiagram [f,g] 2 "state 2" [Connection [1] [2] ""] [1], we can get "f","g" and a start state in it
	// d: [1] => S_1_2Flow from S_1_2 to N_1_2_1 with EmptyTrigger
	S_1_2Flow.from = S_1_2
	S_1_2Flow.label = EmptyTrigger
	S_1_2Flow.to = N_1_2_1
	// d: Connection [1] [2] "" => 1.flow[""] = 2 => f.flow[""] = g => Connection4 from N_1_2_1 to N_1_2_2 with EmptyTrigger
	Connection4.from = N_1_2_1
	Connection4.label = EmptyTrigger
	Connection4.to = N_1_2_2 
	N_1_2_1.name = Name4 // "State 2a"
	N_1_2_2.name = Name5 // "State 2b"
}

run {} for 14
