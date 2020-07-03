

fun apply(f:int*int -> int, x:int, y:int) =
	f(x,y);

fun plus(x:int, y:int) =
	x+y;

apply(plus, 10, 32);

(* val t = (1,2)
val first = #1 t *)
