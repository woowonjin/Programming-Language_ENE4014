fun map (f, xs) =
  case xs of
      [] => []
    | x::xs' => f(x)::map(f, xs')

fun filter(f, xs) = 
  case xs of
    [] => [] 
  | x::xs' => if f x then x::filter(f, xs') 
                     else filter(f, xs')

fun fold(f, acc, xs) = 
    case xs of 
      []     => acc
    | x::xs' => fold(f, (f(acc,x)), xs')


val nums_list = [[9, 40, 75, 7],
                 [64, 34, 88, 96],
                 [91, 92, 53, 31],
                 [50, 84, 73, 65],
                 [54, 44, 75, 11],
                 [91, 71, 48, 46],
                 [70, 72, 5, 42],
                 [25, 77, 49, 56],
                 [89, 4, 73, 52],
                 [36, 56, 61, 1]]

val local_max = map(fn nums => fold(Int.max, hd(nums), nums), nums_list)

val global_max = fold(Int.max, hd(local_max), local_max)

(* given x, count the multiples of x in each list
 * x=11, [[1, 2, 11], [2, 3, 22], [4, 5]]
 * ==>   [1, 1, 0]
 *)
fun count_multiples (x, nums_list) =
    map(length,
        map(fn nums => filter(fn y => (y mod x)=0, nums),
            nums_list))

(* similar to above, given x, count the multiples of x in each list
 * and returns the index of the list having the maximum count.
 * x=11, [[1, 2, 11], [11, 22, 33], [4, 5]]
 * ==>   1  
 *)

fun index_of_max_multiple_count (x, nums_list) =
let val counts = count_multiples(x, nums_list)
in
    fold(fn (x, y) => if (#3 x) > y then ((#1 x), 1+(#2 x), #3 x)
                                  else ((#2 x), 1+(#2 x), y),
        (0, 0, hd(counts)),
        counts)
    
end
