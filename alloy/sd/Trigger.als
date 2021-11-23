// Two methods with implementing triggers, I use the first method in temporary

//method 1
/*abstract sig State{
	triggerwith: set Trigger -> State
}

abstract sig Char{

}

abstract sig Trigger{
	notated: lone Char // if no Char, it is an unconditional trigger
}

fact{
 	no c1: Char | c1 not in Trigger.notated
}
*/

//method 2
abstract sig State{
	triggerwith: set Trigger
}

abstract sig Char{

}

abstract sig Trigger{
	notated: lone Char, // if no Char, it is an unconditional trigger
	aftertrigger: set State
}

fact{
 	no c1: Char | c1 not in Trigger.notated
}
