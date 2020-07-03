val z = 0
fun f(x) =
  let
    val y = x+1
    val x = y-1
  in
    x + y + z
  end

val z = 4
val ans = f 42
