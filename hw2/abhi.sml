(* Dan Grossman, CSE341, HW2 Provided Tests *)
use "hw2.sml";
(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Jack,Clubs),(Num(8),Spades)]
	val moves = [Draw,Discard(Jack,Hearts)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 5 *)
    let val cards = [(Ace,Clubs),(Ace,Spades),(Ace,Clubs),(Ace,Spades)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end

(*---------------My Tests----------------*)

(*---------------Problem 1 Tests--------------*)

(*Tests for remove_option*)
val x1 = remove_option("a", ["a","b","c","d"]) = SOME(["b","c","d"])
val x2 = remove_option("b", ["a","b","c","d"]) = SOME(["a","c","d"])
val x3 = remove_option("c", ["a","b","c","d"]) = SOME(["a","b","d"])
val x4 = remove_option("d", ["a","b","c","d"]) = SOME(["a","b","c"])
val x5 = remove_option("e", ["a","b","c","d"]) = NONE

(*Tests for all_substitutions1 and all_substitutions2*)
val as11 = (all_substitutions1([["Fred", "Frederick"],
			    ["Elizabeth","Betty"],
			    ["Freddie", "Fred", "F"]],"Fred") = ["Frederick","Freddie","F"])

val as21 = (all_substitutions2([["Fred", "Frederick"],
			    ["Elizabeth","Betty"],
			    ["Freddie", "Fred", "F"]],"Fred") = ["Frederick","Freddie","F"])

val as12 = (all_substitutions1([["Fred","Fredrick"],
				["Jeff","Jeffrey"],
				["Geoff","Jeff","Jeffrey"]], "Jeff") =  ["Jeffrey","Geoff","Jeffrey"])

val as22 = (all_substitutions2([["Fred","Fredrick"],
				["Jeff","Jeffrey"],
				["Geoff","Jeff","Jeffrey"]], "Jeff") =  ["Jeffrey","Geoff","Jeffrey"])

val as13 = (all_substitutions1([["Fred","Fredrick"],
				["Jeff","Jeffrey"],
				["Geoff","Jeff","Jeffrey"]], "Abhishek") =  [])

val as23 = (all_substitutions2([["Fred","Fredrick"],
				["Jeff","Jeffrey"],
				["Geoff","Jeff","Jeffrey"]], "Abhishek") =  [])
								    
(*Test for similar_names*)
val sn = similar_names([["Fred","Fredrick"],
		       ["Elizabeth","Betty"],["Freddie","Fred","F"]],
		      {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"},
								   {first="Fredrick", last="Smith", middle="W"},
								   {first="Freddie", last="Smith", middle="W"},
								   {first="F", last="Smith", middle="W"}]


(*---------------Tests for Problem 2-------------------*)


(*Tests for card_color*)

val cc1 = card_color(Num 3, Hearts) = Red

val cc2 = card_color(Num 3, Diamonds) = Red

val cc3 = card_color(Num 3, Clubs) = Black

val cc4 = card_color(Num 3, Spades) = Black

(*Test for card_value*)

(*Cases are explicitly covered in sum_cards*)
					  
(*Test for remove_card*)
fun rc1() = remove_card([(Num 3, Hearts),(Num 5, Diamonds),
		       (Num 6, Spades)],(Num 7, Spades), IllegalMove)
				  
val rc2 = remove_card([(Num 3, Hearts),(Num 5, Diamonds),
		       (Num 6, Spades)],(Num 6, Spades), IllegalMove) =
	  [(Num 3, Hearts), (Num 5, Diamonds)]

val rc3 = remove_card([(Num 3, Hearts),(Num 5, Diamonds),
		       (Num 6, Spades)],(Num 5, Diamonds), IllegalMove) =
	  [(Num 3, Hearts), (Num 6, Spades)]

val rc4 = remove_card([(Num 3, Hearts),(Num 5, Diamonds),
		       (Num 6, Spades)],(Num 3, Hearts), IllegalMove) =
	  [(Num 5, Diamonds), (Num 6, Spades)]

(*Tests for all same color*)
val asc1 = all_same_color([(Num 3, Hearts),(Num 5, Diamonds),(Num 6, Hearts)])

val asc2 = all_same_color([(Num 3, Hearts),(Num 5, Diamonds),(Num 6, Spades)]) = false

val asc3 = all_same_color([])

val asc4 = all_same_color([(Num 5, Hearts)])

(*Tests for sum_cards*)
val sc1 = sum_cards([(Num 3, Hearts),(Num 5, Diamonds),(Num 6, Hearts)]) = 14

val sc2 = sum_cards([]) = 0

val sc3 = sum_cards([(Num 3, Hearts)]) = 3

val sc4 = sum_cards([(Jack, Spades)]) = 10

val sc5 = sum_cards([(Queen, Hearts)]) = 10

val sc6 = sum_cards([(King, Diamonds)]) = 10

val sc7 = sum_cards([(Ace, Spades)]) = 11
									     
(*Tests for score*)
val s1 = score([(Num 3, Hearts),(Num 5, Diamonds),(Num 6, Hearts)], 12) = 5

val s2 = score([(Num 3, Hearts),(Num 5, Diamonds),(Num 6, Hearts)], 16) = 1

val s3 = score([(Num 3, Hearts),(Num 5, Diamonds),(Num 6, Spades)], 12) = 10

val s4 = score([(Num 3, Hearts),(Num 5, Diamonds),(Num 6, Spades)], 16) = 2

