
datatype exp = Constant of int
              | Negate of exp
              | Add of exp*exp
              | Multiply of exp*exp
              | If of bool*exp*exp

fun eval(expr) =
  case expr of
      Constant n => n
    | Negate e => ~ (eval e)
    | Add(e1, e2) => (eval e1) + (eval e2)
    | Multiply(e1, e2) => (eval e1) * (eval e2)
    | If(true, e1, e2) => eval(e1)
    | If(false, e1, e2) => eval(e2)
