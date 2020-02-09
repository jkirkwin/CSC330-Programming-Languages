(*
 * As suggested by the instructor, I'm implementing the String library.
 *)

signature STRING_SIG =
sig 
    val size : string -> int
    val charAt string * int -> char
    val substring : string * int * int -> string
    val concat : string * string -> string
    val concatList : string list -> string 
end

structure stringlib :> STRING_SIG =
struct
    exception EmptyList
    exception BadIndex

    fun size s = 
        case explode s of 
             [] => 0
           | _::s' => 1 + size xs'
    
    fun charAt(s, i) =
        case explode s of 
             [] => raise EmptyList
           | c::s' => if i = 0 then c else charAt(implode s', i-1)
    
    fun subString (s, low, high) =
        if low < 0 orelse high > size s then raise BadIndex
        else 
            let 
                fun skip cs n = 
                    if n <= 0 then s
                    else case cs of 
                        [] => raise EmptyList
                      | _::cs' => skip cs' (n-1)
                
                fun subStringFrom0 cs high =
                    if high < 0 then raise BadIndex
                    else if high = 0 then []
                    else 
                        case cs of 
                            [] => raise EmptyList 
                          | c::cs' => c::subStringFrom0 cs' (high-1)
            in 
                subStringFrom0 (skip (explode s) low) high
            end
       
    fun betterSubString (s, low, high) =
        let 
            fun consIfInRange c acc =
                let 
                    val pos = #1 acc
                    val l = #2 acc 
                    val inRange = low <= pos andalso pos < high
                in
                    if isInRange then (pos+1, c::l) 
                    else (pos+1, l)
        in
            List.foldl consIfInRange (0, []) (explode s)
        end
    
    fun concat (s1, s2) =
        foldl (op ::) s2 (explode s1)

    fun concatList sList =
        case sList of 
            [] => ""
          | s::sList' => concat(s, concatList(sList')
   
end
