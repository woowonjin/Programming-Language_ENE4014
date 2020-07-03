datatype mytype = TwoInts of int*int
                | Str of string
                | Pizza;

fun f(x) = 
  case x of
      Pizza => 3
    | TwoInts(i1, i2) => i1+i2
    | Str s => String.size(s)
