(* CSC 330, Assignment 2 *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* Helper to and together a list of booleans. [] evaluates to false. *)
fun listAnd(bools)=
  case bools of
    [] => false 
  | b::[] => b
  | b::bools' => b andalso listAnd(bools')

(* Question 1
 * Returns NONE if s does not appear in strings, otherwise return strings with s removed.
 *)
fun all_except_option(s: string, strings: string list): string list option =
  case strings of
    [] => NONE
  | first::remaining => 
      if same_string(s, first) 
        then SOME(remaining)
        else 
          let val resultOption = all_except_option(s, remaining)
            in
              case resultOption of
                NONE => NONE (* String not found in remainder of list *)
              | SOME result => SOME(first::result)
            end              

val pattern = "pattern"
val no_matches = ["a", "patter", "b"]
val test1_0 = isSome(all_except_option(pattern, no_matches)) = false 

val final_match = all_except_option(pattern, no_matches@[pattern])
val test1_1 = isSome(final_match) andalso (valOf(final_match) = no_matches)

val initial_match = all_except_option(pattern, pattern::no_matches)
val test1_2 = isSome(initial_match) andalso (valOf(initial_match) = no_matches)

val test1 = listAnd [test1_0, test1_1, test1_2]

(* Question 2
 * Get a list of all strings which exist in one of the lists in substitutions that also
 * constains s (but the result will not contain s itself).
 *)
fun get_substitutions1(substitutions: string list list, s: string): string list =
  case substitutions of
    [] => [] (* Substitutions list of lists is empty *)
  | subList::remainingLists => 
      let
        val firstListOption = all_except_option(s, subList)
        val remainingListsResult = get_substitutions1(remainingLists, s)
      in
        case firstListOption of 
          NONE => remainingListsResult
        | SOME subList => subList@remainingListsResult 
      end

val aList = ["A", "a", "1"]
val bList = ["B", "b", "2"]
val cList = ["C", "c" , "3"]
val subs = [aList, bList, cList, ["!"], ["$$", "$$"]]
val test2_0 = get_substitutions1(subs, "a") = ["A", "1"] 
val test2_1 = get_substitutions1(subs, "!") = []
val test2_2 = get_substitutions1(subs, "$$") = ["$$"]
val test2 = listAnd [test2_0, test2_1, test2_2]

(* Question 3
 * Same as above but using a tail recursive helper
 *)
fun get_substitutions2(substitutions: string list list, s: string): string list =
    (* Tail recursive helper function to do the work. *)
    let fun accumulateSubs(substitutions, acc: string list) =
      case substitutions of 
        [] => acc
      | substitutionList::substitutions' =>
          let
            val subsOption = all_except_option(s, substitutionList)
          in
            case subsOption of
              NONE => accumulateSubs(substitutions', acc)
            | SOME subs => accumulateSubs(substitutions', acc@subs)
          end
    in
      accumulateSubs(substitutions, [])
    end

(* Re-uses the values defined for test 2 *)
val test3_0 = get_substitutions2(subs, "a") = ["A", "1"] 
val test3_1 = get_substitutions2(subs, "!") = []
val test3_2 = get_substitutions2(subs, "$$") = ["$$"]
val test3 = listAnd [test3_0, test3_1, test3_2]

(* Question 4
 * Returns a list of all names similar to the one given by replacing the first 
 * name with any substution given. Duplicates permitted. Result begins with original 
 * name.
 *)
fun similar_names(
    substitutions: string list list, 
    {first:string, middle:string, last:string })=
  let
    val firstNames = first::get_substitutions2(substitutions, first)
    
    (* Helper to create recods using each of the given first names *)
    fun generateNames(firstNames: string list) =
      case firstNames of 
        [] => []
        | head::tail => {first = head, middle = middle, last = last}::generateNames(tail)
  in
    generateNames(firstNames)
  end

val aNames = [
  {first = "a", middle = "m", last = "l"}, 
  {first = "A", middle = "m", last = "l"}, 
  {first = "1", middle = "m", last = "l"}
] 
val test4_0 = similar_names(subs, {first = "a", middle = "m", last = "l"}) = aNames

val bangName = {first = "!", middle = "m", last = "l"}
val test4_1 = similar_names(subs, bangName) = [bangName]

val cashName = {first = "$$", middle = "m", last = "l"}
val test4_2 = similar_names(subs, cashName) = [cashName, cashName]

val test4 = listAnd [test4_0, test4_1, test4_2]

(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove

(* put your solutions for Part 2 here *)

(* Question 5
 * Maps the given card to the appropriate color 
 *)
fun card_color(c: card): color =
  case c of 
    (Clubs, _) => Black
  | (Diamonds, _) => Red 
  | (Hearts, _) => Red 
  | (Spades, _) => Black

val test5_0 = card_color((Hearts, Ace)) = Red
val test5_1 = card_color((Clubs, King)) = Black
val test5_2 = card_color((Diamonds, Num 2)) = Red
val test5_3 = card_color((Spades, Num 10)) = Black

val test5 = listAnd [test5_0, test5_1, test5_2, test5_3]

(* Question 6
 * Maps a card to the appropriate value 
 *)
fun card_value(c: card): int =
  case c of
    (_, Num i) => i
  | (_, Jack) => 10
  | (_, Queen) => 10
  | (_, King) => 10
  | (_, ACE) => 11

val test6_0 = card_value((Hearts, Ace)) = 11
val test6_1 = card_value((Clubs, King)) = 10
val test6_2 = card_value((Spades, Queen)) = 10
val test6_3 = card_value((Diamonds, Jack)) = 10
val test6_4 = card_value((Spades, Num 10)) = 10
val test6_5 = card_value((Diamonds, Num 2)) = 2

val test6 = listAnd [test6_0, test6_1, test6_2, test6_3, test6_4, test6_5]

(* Question 7 
 * Removes the card from the list. 
 * If the card appears multiple times, removes only the first ocurrance.
 * If c is not in the list, raise the given exception.
 *)
fun remove_card(cs, c, e)=
  case cs of 
    [] => raise e
  | first::cs' => 
      if first = c 
      then cs'
      (* Opted not to implement as tail recursive since this is more 
      readable and less complex *)
      else first::remove_card(cs', c, e)

exception testExcept
val cards = [(Hearts, Ace), (Hearts, King), (Hearts, Queen), (Hearts, Jack)]
val test7_0 = remove_card([], (Hearts, Num 10), testExcept) = [] andalso false handle testExcept => true;
val test7_1 = remove_card([(Hearts, Num 10)], (Hearts, Num 10), testExcept) = [] 
val test7_2 = remove_card(cards, (Hearts, Jack), testExcept) = [(Hearts, Ace), (Hearts, King), (Hearts, Queen)]

val test7 = listAnd [test7_0, test7_1, test7_2]


(* Question 8
 * True if all cards are the same color.
 * True if list given is empty.
 *)
fun all_same_color(cs)=
  case cs of 
    [] => true
  | _::[] => true
  | c1::(c2::cs') => card_color(c1) = card_color(c2) andalso all_same_color(c2::cs')

val reds = [(Diamonds, Num 8), (Hearts, Ace), (Hearts, Jack), (Diamonds, Num 3)] 
val test8_0 = all_same_color(reds)
val test8_1 = all_same_color([])
val test8_2 = all_same_color((Spades, Queen)::reds) = false
val test8_2 = all_same_color(reds@[(Spades, Queen)]) = false

val test8 = listAnd [test8_0, test8_1, test8_2, test8_2]

(* Question 9
 * Gives the sum of the list of cards. 
 * Use a tail recursive helper function. 
 *)
fun sum_cards(cs)=
  let 
    fun accumulateSum(sum, cs) =
      case cs of 
        [] => sum
      | c::cs' => accumulateSum(sum + card_value(c), cs')
  in
    accumulateSum(0, cs)
  end

val tenHandClubs = [(Clubs, Num 1), (Clubs, Num 2), (Clubs, Num 3), (Clubs, Num 4)]
val test9_0 = sum_cards(tenHandClubs) = 10
val test9_1 = sum_cards((Spades, King)::tenHandClubs) = 20
val test9_2 = sum_cards([]) = 0

val test9 = listAnd [test9_0, test9_1, test9_2]

(* Question 10
 * Computes the score given the cards and goal.
 *)
fun score(cs, goal)=
  let 
    fun preliminaryScore(sum, goal)=
      if sum > goal 
      then 2 * (sum - goal)
      else goal - sum
    val prelim = preliminaryScore(sum_cards(cs), goal)
  in
    if all_same_color(cs) 
    then prelim div 2
    else prelim
  end    

val tenHandMixed = [(Diamonds, Num 1), (Clubs, Num 2), (Spades, Num 3), (Clubs, Num 4)]
val test10_0 = score(tenHandMixed, 10) = 0
val test10_1 = score(tenHandMixed, 9) = 2
val test10_2 = score(tenHandMixed, 12) = 2
val test10_3 = score(tenHandClubs, 12) = 1

val test10 = listAnd [test10_1, test10_2, test10_3]

(* Question 11
 * Runs the game specified by the given list of cards, list of moves, and goal. 
 * Returns the score of the game after processing the provided moves.
 * Raises IllegalMove if the given move list is invalid in conjunction with the cards. 
 *)
fun officiate(cs, moves, goal)=
  let
    fun processMoves(heldCards, moveList, cardList): int =
      case moveList of
        [] => score(heldCards, goal)
      | (Discard c)::moveList' => processMoves(remove_card(heldCards, c, IllegalMove),  moveList', cardList) 
      | Draw::moveList' => 
          case cardList of
            [] => score(heldCards, goal)
          | c::cardList' => 
              let val heldCards' = c::heldCards
              in 
                if sum_cards(heldCards') > goal then score(heldCards', goal)
                else processMoves(heldCards', moveList', cardList')
              end
  in
    processMoves([], moves, cs)
  end

val test11_0 = officiate([], [], 0) = 0
val test11_1 = officiate([], [Discard (Spades, Queen)], 1) = ~1 handle IllegalMove => true
val test11_2 = officiate([(Hearts, Ace)], [Draw, Discard (Hearts, Ace)], 0) = 11
val test11_3 = officiate([(Hearts, Num 1)], [Draw, Discard (Hearts, Num 1)], 2) = 1

val test11 = listAnd [test11_0, test11_1, test11_2, test11_3]

val tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11]