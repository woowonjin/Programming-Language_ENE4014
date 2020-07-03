datatype my_list = Empty
                  | Cons of int * my_list

val x = Cons(4, Cons(22+1, Cons(2017, Empty)))

fun append_my_list(xs, ys) =
  case xs of
    Empty => ys
  | Cons(n, xs') => Cons(n, append_my_list(xs', ys))

fun inc_or_zero intoption =
  case intoption of
      NONE => 0
    | Some i => i+1
