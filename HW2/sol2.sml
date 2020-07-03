datatype expr = NUM of int
              | PLUS of expr * expr
              | MINUS of expr * expr

datatype formula = TRUE
                | FALSE
                | NOT of formula
                | ANDALSO of formula * formula
                | ORELSE of formula * formula
                | IMPLY of formula * formula
                | LESS of expr * expr

fun exp(e:expr) =
  case e of
        NUM i => i
      | PLUS(e1, e2) => exp(e1) + exp(e2)
      | MINUS(e1, e2) => exp(e1) - exp(e2)

fun eval(e:formula) = 
    case e of
          TRUE => true
        | FALSE => false
        | NOT i => if i = TRUE
                   then eval(FALSE)
                   else eval(TRUE)
        | ANDALSO(e1, e2) => (eval e1) andalso (eval e2)
        | ORELSE(e1, e2) => (eval e1) orelse (eval e2)
        | IMPLY(e1, e2) => (eval(NOT e1)) orelse (eval e2)
        | LESS(e1, e2) => exp(e1) < exp(e2)

type name = string

datatype metro = STATION of name
              |  AREA of name * metro
              |  CONNECT of metro * metro

fun checkStationWithArea(area: name list, e:metro) =
    let
        fun checkStationInList(area, station : name) =
            if(null(area))
            then false
            else (hd(area) = station) orelse checkStationInList(tl(area), station)
    in
      case e of
        STATION e1 => checkStationInList(area, e1)
      | AREA(name1,e1) => checkStationWithArea(name1::area, e1)
      | CONNECT(e1,e2) => checkStationWithArea(area, e1) andalso checkStationWithArea(area, e2)
    end
fun checkMetro(e:metro) =
    case e of
        STATION e1 => false
      | AREA(name,e1) => checkStationWithArea(name::[], e1)
      | CONNECT(e1,e2) => checkMetro(e1) andalso checkMetro(e2)


datatype 'a lazyList = nullList
                    |  cons of 'a * (unit -> 'a lazyList)

fun seq(first:int, last:int) =
  if first < last
  then cons(first, fn()=>seq(first+1,last))
  else nullList

fun infSeq(first:int) =
  cons(first, fn()=>infSeq(first+1))

fun firstN(cons(x,xs), n) = 
                if n = 0
                then []
                else x :: firstN(xs(), n-1)
  | firstN(nullList, n) = []

fun Nth(nullList, n) = NONE
  | Nth(cons(x, xs), n) = 
          if n = 1
          then SOME x
          else Nth(xs(), n-1)

fun filterMultiples(nullList, n) = nullList
  | filterMultiples(cons(x,xs), n) = if (x mod n) = 0
                                     then filterMultiples(xs(), n)
                                     else cons(x,fn()=>filterMultiples(xs(), n))

fun primes() =
  let
    fun sieve(lazyListVal) =
      case lazyListVal of
          nullList => nullList
      |   cons(x,xs) => cons(x, fn()=>sieve(filterMultiples(xs(), x)))
  in
    sieve(infSeq(2))
  end
