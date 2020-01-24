(* Assign 03 Provided Code *)

(*  Version 1.0 *)

(* Jamie Kirkwin, CSC 330, Assignment 3 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

(* Description of g:
g takes two functions, f1 and f2, and a pattern p and recursively traverses p 
until it has been decomposed into its atomic components. 

f1 is called once for each wildcard encountered, and must return and int. 
f2 is called once for each named variable encountered and is passed the name 
of that variable. f2 must also return an int.

g returns the sum of the results of all calls to f1 and f2.
*)

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** put all your code after this line ****)

(* Question 1 *)
fun only_capitals(xs) =
  List.filter (fn s => Char.isUpper(String.sub(s,0))) xs

(* Question 2 *)
fun longest_string1(xs) =
  let
    val f = fn (s, acc) => if String.size(s) > String.size(acc) then s else acc
  in
    foldl f "" xs 
  end

(* Question 3 *) 
fun longest_string2(xs) =
  let
    val f = fn (s, acc) => if String.size(s) >= String.size(acc) then s else acc
  in
    foldl f "" xs 
  end

(* Question 4 *)
fun longest_string_helper compare xs =
  let 
    val f = fn (s, acc) => if compare(String.size(s), String.size(acc)) then s else acc
  in 
    foldl f "" xs
  end

val longest_string3 =
  longest_string_helper (op >)

val longest_string4 =
  longest_string_helper (op >=)

(* Question 5 *)
val longest_capitalized = longest_string3 o only_capitals 

(* Question 6 *)
val rev_string = implode o rev o explode

(* Question 7 
Write a function first_answer that has type (’a -> ’b option) -> ’a list -> ’b.
The first argument should be applied to elements of the second argument in order, until
the first time it returns SOME v for some v and then v is the result of the call
to first_answer. 
If the first argument returns NONE for all list elements, then first_answer should raise
the exception NoAnswer. 
*)
fun first_answer f xs = 
  case List.filter (isSome o f) xs of 
      [] => raise NoAnswer
    | x::xs' => valOf (f x) (* This is guaranteed to be same due to the filter. *)

(* Question 8
Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list
option (notice the 2 arguments are curried).

The first argument should be applied to elements of the second argument. 
If it returns NONE for any element, then the result for all_answers is NONE. 
Else the calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn 
and the result of all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together

(the order in the result list should be preserved).

`all_answers f []` should evaluate to SOME [].
*)
fun all_answers f xs = 
  let 
    fun helper(x, acc) =
      let val result = f x 
      in
        case (acc, result) of 
          (SOME accList, SOME resultList) => SOME (accList@resultList)
        | _ => NONE
      end 
  in
    foldl helper (SOME []) xs
  end

(* Question 9 *)
val count_wildcards = g (fn () => 1) (fn s => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn s => String.size(s)) 

fun count_some_var (s, p) = 
  g (fn () => 0) (fn v => if s=v then 1 else 0) p

(* Question 10 

Write a function check_pat that takes a pattern and returns true if and only if all the variables
appearing in the pattern are distinct from each other (i.e., use different strings). 

The constructor names are not relevant. 
*)
fun check_pat pattern = 
  let
    fun getAllNames(p, acc) = 
	    case p of
	      Wildcard            => acc
	    | Variable x          => acc@[x]
	    | TupleP ps           => List.foldl getAllNames acc ps 
	    | ConstructorP(_, p)  => getAllNames(p, acc)
	    | _                   => acc

    fun hasRepeats xs =
      case xs of 
        [] => false
      | x::xs' => List.exists (fn s => x=s) xs' orelse hasRepeats xs'
  in
    (not o hasRepeats o getAllNames)(pattern, [])
  end

(* Question 11 
Write a function match that takes a valu * pattern and returns a (string * valu) list option. 
    NONE if the pattern does not match 
    SOME l where l is the list of bindings if it does. 

Note that if the value matches but the pattern has no patterns of the form <Variable s>, 
then the result is SOME []. 
*)
fun match(v, p) =
  case (v, p) of 
      (_, Wildcard) => SOME []
    | (v, Variable s) => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const i, ConstP j) => if i=j then SOME [] else NONE
    | (Tuple vs, TupleP ps) =>
          if List.length(ps) <> List.length(vs) then NONE
          else all_answers match (ListPair.zip(vs, ps))
    | (Constructor(s1, v), ConstructorP(s2, p)) =>
      if s1 <> s2 then NONE
      else match(v, p)
      (* todo unsure if we need to have a binding s, [list of bindings]  or not *)
    | _ => NONE

(* Question 12 
Write a function first_match that takes a value and a list of patterns and returns a 
(string * valu) list option
    NONE if no pattern in the list matches or 
    SOME lst where lst is the list of bindings for the first pattern in the list that matches. 
*)
fun first_match v ps =
  SOME (first_answer (fn p => match(v,p)) ps) 
  handle NoAnswer => NONE