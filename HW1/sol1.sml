(* 1 *)
fun merge(arr1:int list, arr2:int list) =
  if null arr1
  then arr2
  else if null arr2
       then arr1
       else if hd(arr1) < hd(arr2)
            then hd(arr1) :: merge(tl(arr1), arr2)
            else hd(arr2) :: merge(arr1, tl(arr2))

(* 2 *)
fun reverse(arr:int list) =
  let
    fun reverse_help(arr1:int list, arr2:int list) =
      if null(tl(arr1))
      then hd(arr1) :: arr2
      else reverse_help(tl(arr1), hd(arr1)::arr2)
  in reverse_help(arr, [])
  end

(* for test *)(*
fun test(x:int) =
  x
*)

(* 3 *)
fun sigma(a:int, b:int, f:int->int) =
  if a > b
  then 0
  else f(a) + sigma(a+1, b, f)

(* 4 *)
fun digits(x: int) =
  let
      fun digit_help(target: int, division: int) =
        if target div division < 10
        then if target < 10
             then [target]
             else target div division :: digit_help(target mod division, division div 10)
        else digit_help(target, division*10) 
  in
    digit_help(x, 10)
  end


(* 5 *)
fun digitalRoot(x: int) =
  let
    fun addElement(arr:int list) =
      if null(tl(arr))
      then hd(arr)
      else hd(arr) + addElement(tl(arr))
    val sum = addElement(digits(x))
  in
    if sum > 9
    then digitalRoot(sum)
    else sum
  end


fun additivePersistence(x: int) =
  let
    fun addElement(arr:int list) =
      if null(tl(arr))
      then hd(arr)
      else hd(arr) + addElement(tl(arr))
    val sum = addElement(digits(x))
  in
    if sum > 9
    then 1 + additivePersistence(sum)
    else 1
  end
