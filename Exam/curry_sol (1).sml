


fun curry f x y = f (x,y)

fun uncurry f (x,y) = f x y

fun other_curry1 f = fn x => fn y => f y x

fun other_curry2 f x y = f y x

(* example *)

(* tupled but we wish it were curried *)
fun range (i,j) = if i > j then [] else i :: range(i+1, j)

(* no problem *)
val count_upto = curry range 1

val xs = count_upto 7

val count10from = other_curry2 (curry range) 10
