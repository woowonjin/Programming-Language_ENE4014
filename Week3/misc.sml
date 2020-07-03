fun sum_list xs = (* xs: int list *)
  case xs of
      [] => 0
    | x::xs' => x+sum_list(xs')

