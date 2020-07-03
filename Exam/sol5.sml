datatype exp = Constant of int | Negate of exp
| Add of exp * exp
| Multiply of exp * exp

fun true_of_some_constants f e m =
  let 
    fun count_of_true_constants(f, e) =
      case e of
        Constant i => if f i
                      then 1
                      else 0
    |   Negate e1 => count_of_true_constants(f, e1)
    |   Add(e1,e2) => count_of_true_constants(f, e1) + count_of_true_constants(f, e2)
    |   Multiply(e1,e2) => count_of_true_constants(f, e1) + count_of_true_constants(f, e2)
  in
    if m = count_of_true_constants(f, e)
    then true
    else false
  end

val some_even_exp = true_of_some_constants (fn x => x mod 2 = 0)
val e = Multiply( Add( Constant(2), Constant(1) ), Constant(4) )
(*some_even_exp e 3 (* false *)
some_even_exp e 2 (* true *)*)
