
fun swap(pr: int*bool) =
	(* we want to swap the pr
	 e.g. pr (10, true) --> (true, 10) *)
	(#2 pr , #1 pr)

fun sum_two_pairs(pr1: int*int, pr2: int*int) =
	(* we want to sum pr1, pr2 *)
	 (#1 pr1 + #1 pr2, #2 pr1 + #2 pr2)
	
fun div_mod(x:int, y:int) =
	(x div y, x mod y)

fun sort_pair(pr: int*int) =
	if #1 pr < #2 pr
	then pr
	else (#2 pr, #1 pr)
