use "quick-sort.sml";

(* Review for midterm 1
    Exercise: write functions to convert between a tree and a list
    
    I've decided we'll just do a generic list and a generic tree. 
    For this to work we just need a comparator function.
 *)


datatype 'a Node = 
    Parent of ('a Node * 'a * 'a Node) |
    Leaf of 'a |
    Empty (* Option syntax was super ugly when creating trees. *)

(* Returns a list of the elements of the tree given by the root node *)
fun treeToList root = 
    case root of
        Empty => []
        |
        Leaf x => [x]
        | 
        Parent (leftChild, x, rightChild) => treeToList(leftChild) @ [x] @ treeToList(rightChild)

(*  Returns a binary tree of the elements of the given list, 
    using the provided function to determine order *)
fun listToTree lessThan xs =
    let 
        fun curry f x y = f(x, y)

        fun toTree xs =
            case xs of 
                [] => Empty
                |
                x::xs' => 
                    let
                        val rightSubTree = toTree (List.filter (curry lessThan x)  xs')
                        val leftSubTree = toTree (List.filter  (not o (curry lessThan x)) xs')
                    in
                        case (rightSubTree, leftSubTree) of
                            (Empty, Empty) => Leaf(x)
                            |
                            _ => Parent(leftSubTree, x, rightSubTree)
                end
    in 
        toTree xs
    end
                
