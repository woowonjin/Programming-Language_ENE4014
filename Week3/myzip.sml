exception ListLengthMismatch

fun zip3 lists =
  case lists of 
      ([],[],[]) => []
    | (x::xs, y::ys, z::zs) => (x,y,z) :: zip3(xs, ys, zs)
    | _ => raise ListLengthMismatch
