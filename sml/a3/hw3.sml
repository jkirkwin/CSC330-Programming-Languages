(* Assign 03 Provided Code *)

(*  Version 1.0 *)

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
  List.filter(fn s => Char.isUpper(String.sub(s,0)), xs)

(* Question 2 *)
fun longest_string1(xs) =
  let
    val f = fn (s, acc) => if String.size(s) > String.size(acc) then s else acc
  in
    foldl f  "" xs 
  end

(* Question 3 *) 

(* Question 4 *)
fun longest_string_helper f xs =
  foldl(f, "", xs)

val longest_string3 =
  1
    

fun longest_string4() =
  1

(* Question 5 *)
val longest_capitalized = longest_string o only_capitals 


(* Question 6 *)
fun rev_string s
  ""


(* Question 7 *)


(* Question 8 *)


(* Question 9 *)


(* Question 10 *)


(* Question 11 *)


(* Question 12 *)
