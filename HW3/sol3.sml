datatype pattern = Wildcard | Variable of string | UnitP 
                  | ConstP of int | TupleP of pattern list
                  | ConstructorP of string * pattern 

datatype valu = Const of int | Unit | Tuple of valu list
              | Constructor of string * valu

fun check_pat(p:pattern) =
  let
    fun helper1(p1:pattern) =
      case p1 of 
        Wildcard => []
      | Variable s => s :: []
      | UnitP => []
      | ConstP i => []
      | TupleP ps => foldl(fn (x,y)=> helper1(x)@y) [] ps
      | ConstructorP(s,p2) => helper1(p2)
    
    fun helper2(strlist) =
      case strlist of
        [] => false
      | x :: xs => (List.exists(fn y => y=x) xs) orelse helper2(xs)
  in
    not (helper2(helper1(p)))
  end

fun match(v:valu, p:pattern) =
  case (v,p) of 
       (_, Wildcard) => SOME []
     | (_ ,Variable s) => SOME ( (s,v)::[] )
     | (Unit, UnitP) => SOME []
     | (Const y,ConstP x) => if x = y
                             then SOME []
                             else NONE
     | (Tuple vs, TupleP ps) => 
        if List.length(vs) = List.length(ps)
        then
             let 
               val vps = ListPair.zip(vs,ps)
               val matchvps = List.filter (fn x => match(x) <> NONE) vps
             in  
               if List.length(ps) = List.length(matchvps)
               then SOME( foldl ( fn(x,y) => valOf(match(x)) @ y) [] vps )
               else NONE
             end
        else NONE
     | (Constructor (s2,v), ConstructorP (s1,p) ) =>
         if s1 = s2 
         then match (v,p)
         else NONE
     | _ => NONE           

type name = string
datatype RSP = 
    ROCK
  | SCISSORS
  | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament =
    PLAYER of name * (RSP strategy ref)
  | MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

fun whosWinner(t) =
  let
    fun next(strategyRef) =
      let val Cons(rsp, func) = !strategyRef
      in
        strategyRef := func();
        rsp
      end
    
    fun winRule(rsp1:RSP, rsp2:RSP) =
      case (rsp1, rsp2) of
        (ROCK, SCISSORS) => SOME rsp1
      | (ROCK, PAPER) => SOME rsp2
      | (SCISSORS, ROCK) => SOME rsp2
      | (SCISSORS, PAPER) => SOME rsp1
      | (PAPER, ROCK) => SOME rsp1
      | (PAPER, SCISSORS) => SOME rsp2
      | _ => NONE

    fun game(p1, p2) =
      case (p1, p2) of
        (PLAYER(p1name, strategy1), PLAYER(p2name, strategy2)) => 
          let
            val rsp1 = next(strategy1)
            val rsp2 = next(strategy2)
            val winner = winRule(rsp1, rsp2)
          in
            if winner = NONE
            then game(p1, p2)
            else 
              if valOf(winner) = rsp1
              then p1
              else p2
          end
  in
    case t of
      PLAYER(name, _) => t
    | MATCH(t1, t2) => game(whosWinner(t1), whosWinner(t2))
  end

val match = MATCH(
MATCH(PLAYER("John", ref sr), PLAYER("Steve", ref s)),
         MATCH(PLAYER("Alice", ref p),
           MATCH(PLAYER("David", ref r),
             MATCH(PLAYER("Bill", ref s), PLAYER("Emily", ref srp)))))
