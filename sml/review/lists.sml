exception BadIndex

fun foldl f acc xs =
    case xs of 
        [] => acc 
      | x::xs' => foldl f (f(x, acc)) xs'

fun takeSimple(xs, i) = 
    case (xs, i) of
        (_, 0) => []
    |   (x::xs', n) => x::takeSimple(xs', n-1)
    |   _ => raise BadIndex

fun takeTailRecursive (xs, i) =
    let fun helper xs i acc =
        case (xs, i) of
            (_, 0) => acc
        |   (x::xs', n) => helper xs' (n-1) (acc @ [x])
        | _ => raise BadIndex
    in helper xs i []
    end


fun takeFold (xs, i) =
    let 
        fun length xs = 
            case xs of 
                [] => 0 | _::xs' => 1 + length xs'
        
        fun f (x, acc) = 
            if length acc < i then acc@[x]
            else acc
    in 
        foldl f [] xs
    end 


fun concatL xs = foldl (fn (x, acc) => acc@x) [] xs

fun concatR xs = foldr (fn (x, acc) => x@acc) [] xs

fun map f xs =
    let fun helper xs acc = 
        case xs of 
            [] => acc 
        |   x::xs' => helper xs' (acc@[f x])
    in helper xs []
    end

fun findTailRec f xs =
    case xs of 
        [] => NONE
      | x::xs' => if f x then SOME x else findTailRec f xs'

fun filter f xs = 
    foldl (fn (x,acc) => if f x then acc@[x] else acc) [] xs

fun findFilter f xs =
    case filter f xs of
        [] => NONE 
     |  x::xs' => SOME x