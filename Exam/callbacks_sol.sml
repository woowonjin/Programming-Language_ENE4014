(* private function/variable in our library *)
val cbs : (int -> unit) list ref = ref []

(* internal function *)
fun onEvent i =
   let fun loop fs =
        case fs of 
          [] => ()
        | f::fs' => (f i; loop fs')
    in loop (!cbs) end

(* public interface of our library *)
fun registerKeyEventFunc f = cbs := f::(!cbs)


(* example of client code. 
   notice the two callbacks use bindings of different types (int ref and int)
 *)
val timesPressed = ref 0
val _ = registerKeyEventFunc (fn _ => timesPressed := (!timesPressed) + 1)

fun printIfPressed i =
    registerKeyEventFunc (fn j => if i=j
                        then print ("\027[35;5myou pressed " ^ Int.toString i ^
                        "\027[0m\n")
                        else ())

val _ = printIfPressed 4
val _ = printIfPressed 11
val _ = printIfPressed 23
val _ = printIfPressed 4
