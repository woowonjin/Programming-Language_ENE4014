fun nondecreasing xs =
  case xs of
        [] => true
      | x :: [] => true
      | x::y::rest => (x <= y andalso nondecreasing (y::rest))
