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
	S.flow[EmptyTrigger] = C_1 // picture1: [1] => S.flow[EmptyTrigger] = C_1
	no S.flow[TriggerNames] // Explict added
	C_1.flow[T1] = N_2 // According to the label, we can replace Connection[1] [2] "t" by Connection a b "t", it can be transformed into a.flow[t] = b, namely C_1.flow[T1] = N_2
	no C_1.flow[Triggers - T1] // No other flows like this should be added explictly 
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
	S_1.flow[EmptyTrigger] = N_1_1 // a: [1] => S_1.flow[EmptyTrigger] = N_1_1
	no S_1.flow[TriggerNames] // Explict added
	N_1_1.flow[EmptyTrigger] = C_1_2 // a: Connection [1] [2] "" => 1.flow[""] = 2 => c.flow[""] = d => N_1_1.flow[EmptyTrigger] = C_1_2
	no N_1_1.flow[Triggers - EmptyTrigger] // Explict added
	N_2.flow[EmptyTrigger] = H_1_3 // picture1: Connection[2] [1,3] "" => 2.flow[""] = 1.3 => b.flow[""] = e => N_2.flow[EmptyTrigger] = H_1_3
	no N_2.flow[Triggers - EmptyTrigger] // Explict added
	no C_1_2.flow // Explict added
	no H_1_3.flow // Explict added
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
	S_1_2.flow[EmptyTrigger] = N_1_2_1 // d: [1] => S_1_2.flow[EmptyTrigger] = N_1_2_1
	no S_1_2.flow[TriggerNames] // Explict added
	N_1_2_1.flow[EmptyTrigger] = N_1_2_2 // d: Connection [1] [2] "" => 1.flow[""] = 2 => f.flow[""] = g => N_1_2_1.flow[EmptyTrigger] = N_1_2_2
	no N_1_2_1.flow[Triggers - EmptyTrigger] // Explict added
	no N_1_2_2.flow // Explict added
	N_1_2_1.name = Name4 // "State 2a"
	N_1_2_2.name = Name5 // "State 2b"
}

run {} for 7
