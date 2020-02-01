(* Less than should be a that returns true iff the second 
element is determined to be ordered after of the first.
e.g. If comparing ints, it would retun true if param1 < param2 *)
fun sort lessThan xs =
    let 
        fun curry f x y = 
            f(x, y)

        fun quickSort xs =
            case xs of 
                [] => []
                |
                x::xs' => 
                    let 
                        val greaterThanPivot = (curry lessThan) x
                        val greaterList = List.filter greaterThanPivot xs'
                        val lessList = List.filter (not o greaterThanPivot) xs'
                    in 
                        (quickSort lessList) @ (x::(quickSort greaterList))
                    end
    in quickSort xs
    end