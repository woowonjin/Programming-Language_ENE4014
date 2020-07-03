fun silly1 (z:int) =
  let val x = if z > 0 then z else 34
      val y = x+z+9
  in
      if x > y then x*2 else y*y
  end

silly1(4);
