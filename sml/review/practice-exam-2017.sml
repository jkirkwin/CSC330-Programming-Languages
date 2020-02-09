(* Some code for the practice exam posted from 2017. 
   This is necessary to check my answers; the actual 
   code was initially written on paper. *)

(* Q1-a *)
fun fA x y z = 
    x (y) + z
val y = 3
fun g z = 
    let 
        val x = fn x => x * 2
    in 
        fA z
    end
val h = g (fn a => a * a)

val ansA = h 5 2

(* Q1-b *)
fun fB x = 
    case x of 
        [] => 0 |
        (a,b)::[] => a + b |
        (a,b)::(c,d)::e => a + d + fB(e)
val ansB = fB [(1,2), (1,3), (1,4)]

(* Q1-c *)
val x = 7
fun g y = x * y
fun fC z =
    let
        val x = 3
    in
        g(z) + x
    end
val ansC = fC(2)

(* Q2-a *)
fun addAllOpt is = 
    case is of 
        [] => NONE 
      | NONE::is' => addAllOpt(is')
      | (SOME i) :: is' => 
            let val resultOption = addAllOpt(is')
            in 
                case resultOption of 
                    NONE => SOME i 
                    | SOME result => SOME (i + result)
            end

(* Q2-b *)
fun fold (f, acc, xs) =
    case xs of 
        [] => acc |
        x :: xs' => fold (f, f(acc, x), xs')

fun map(f, xs) =
    let fun appendMapped (acc, x) = acc @ [f x]
    in fold(appendMapped, [], xs)
    end

(* Q3-a *)
datatype tree = 
        EmptyT
      | Tree of (int * tree * tree)

fun insert(tr, value) =
    case tr of 
        EmptyT => Tree (value, EmptyT, EmptyT)
      | Tree (i, left, right) => 
            if value <= i 
            then Tree (i, insert(left, value), right)
            else Tree (i, left, insert(right, value)) 

(* Q3-b *)
fun fold_tree f acc t =
    let val foldFunc = fold_tree f
    in 
        case t of 
            EmptyT => acc
        | Tree(i, left, right) =>
                let 
                    val leftFold = fold_tree f acc left
                    val newAcc = f(leftFold, i)
                in 
                    fold_tree f newAcc right
                end
    end

(* Q3-c *)
fun to_string(acc:string, i:int):string =
    if acc = ""
    then Int.toString(i)
    else acc ^ " " ^ Int.toString(i)

val treeToString = fold_tree to_string ""

(* Q4 *)
fun fib n =
    let fun nextFib(prev, this, n) =
        if n <= 0 then this 
        else nextFib(this, this + prev, n-1)
    in 
        nextFib(0,1,n)
    end