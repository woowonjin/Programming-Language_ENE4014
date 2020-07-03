fun merge (fl: int list, sl: int list) =
        if null fl
        then sl
        else
                if hd(fl) > hd(sl)
                then hd(sl) :: merge(fl, tl(sl))
                else hd(fl) :: merge(tl(fl), sl)
(*
fun reverse (xs: int list) =
        if null xs
        then NONE
        else
                let     fun comp (last: int list) =
                                if null list
                                then NONE
                                else hd(last)
                in

                end
*)

fun sigma (a: int, b: int, f: int->int) =
        if a = b
        then f(a)
        else
                f(a) + sigma(a+1, b, f)


fun digits (dg: int) =
        let
                fun comp(a:int, b:int) =
                        if (b div a) > 9
                        then comp(10*a, b)
                        else
                                if( a > 1 )
                                then (b div a) :: comp((a div 10), (b - (b div a)*a))
                                else (b div a) :: []
        in
                comp(1, dg)
        end
(*
fun additivePersistence (ap: int) =
        val list = digts(n)
        val plus = 1
        let
                fun sum(a: int list) =
                        if null tl(a)
                        then a
                        else (hd(a) + sum(tl(a)))
        in
                if (sum(list) div 10) = 0
                then plus
                else plus + additivePersistence(sum(list))
        end
*)
fun digitalRoot (n: int) =
        val list = digits(n)
        let
                fun sum(a: int list) =
                        if null tl(a)
                        then a
                        else (hd(a) + sum(tl(a)))
        in
                if (sum(list) div 10) = 0
                then sum(list)
                else digitalRoot(sum(list))
        end
