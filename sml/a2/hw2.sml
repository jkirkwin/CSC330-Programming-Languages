(* CSC 330, Assignment 2 *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

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

(* Question 8
 * True if all cards are the same color.
 * True if list given is empty.
 *)
fun all_same_color(cs)=
  case cs of 
    [] => true
  | _::[] => true
  | c1::(c2::cs') => card_color(c1) = card_color(c2) andalso all_same_color(c2::cs')

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
      | Discard c::moveList' => processMoves(remove_card(heldCards, c, IllegalMove),  moveList', cardList) 
      | Draw::moveList' => 
          case cardList of
            [] => score(heldCards, goal)
          | c::cardList' => 
              if sum_cards(c::heldCards) > goal then score(c::heldCards, goal)
              else processMoves(c::heldCards, moveList', cardList')
  in
    processMoves([], moves, cs)
  end