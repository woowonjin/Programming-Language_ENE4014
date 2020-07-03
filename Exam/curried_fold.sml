fun map f xs =
  case xs of
      [] => []
    | x::xs' => f(x)::(map f xs')

fun filter f xs = 
  case xs of
    [] => [] 
  | x::xs' => if f x then x::(filter f xs')
                     else filter f xs'

fun fold f acc xs = 
    case xs of 
      []     => acc
    | x::xs' => fold f (f(acc,x)) xs'


val nums_list = [[9, 40, 75, 7],
                 [64, 34, 88, 96],
                 [91, 92, 53, 31],
                 [50, 84, 73, 65],
                 [54, 44, 75, 11],
                 [91, 71, 48, 46],
                 [70, 72, 5, 42],
                 [89, 4, 73, 52],
                 [36, 56, 61, 1],
                 [25, 77, 49, 59]]

exception InvalidArgument
fun max xs =
    case xs of
      [] => raise InvalidArgument
     |x::[] => x
     |x::xs' => Int.max(x, max xs') ;


val local_max = map max nums_list;

val global_max = max local_max;

(* given x, count the multiples of x in each list
 * x=11, [[1, 2, 11], [2, 3, 22], [4, 5]]
 * ==>   [1, 1, 0]
 *)
fun count_multiples x nums_list =
    map length (map (filter fn y => (y mod x)=0) nums_list)






