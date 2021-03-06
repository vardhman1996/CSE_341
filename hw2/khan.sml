(* Dan Grossman, CSE341, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)
use "hw2provided.sml";

val lststr = ["a", "b", "c", "d", "b"];
remove_option("c", lststr) = SOME(["a", "b", "d", "b"]);
remove_option("b", lststr) = SOME(["a", "c", "d", "b"]);

val lstlststr = [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]];
val lstlststr2 = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]];
all_substitutions1(lstlststr, "Jeff") = ["Jeffrey","Geoff","Jeffrey"];
all_substitutions1(lstlststr2, "Fred") = ["Fredrick","Freddie","F"];
all_substitutions1(lstlststr2, "Betty") = ["Elizabeth"];
all_substitutions1(lstlststr, "none") = [];

all_substitutions2(lstlststr, "Jeff") = ["Jeffrey","Geoff","Jeffrey"];
all_substitutions2(lstlststr2, "Fred") = ["Fredrick","Freddie","F"];
all_substitutions2(lstlststr2, "Betty") = ["Elizabeth"];
all_substitutions2(lstlststr, "none") = [];

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],{first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"},
{first="Fredrick", last="Smith", middle="W"},
{first="Freddie", last="Smith", middle="W"},
{first="F", last="Smith", middle="W"}];

val card1 = (Queen, Clubs);
val card2 = (Num(5),Hearts);
val card3 = (Ace,Diamonds);
val card4 = (King, Diamonds);
val lstcard = [card1, card2, card3];
val lstcard2 = [card2, card3, card4];
card_color(card1) = Black;
card_color(card2) = Red;
card_color(card3) = Red;

card_value(card1) = 10;
card_value(card2) = 5;
card_value(card3) = 11;

remove_card(lstcard, card2, IllegalMove) = [card1, card3];

all_same_color(lstcard) = false;
all_same_color(lstcard2) = true;							      

sum_cards(lstcard) = 10 + 5 + 11 ;
sum_cards(lstcard2) = 5+11+10;
sum_cards([]) = 0;

score(lstcard, 30) = 4;
score(lstcard, 25) = 5 * (26-25);
score(lstcard2, 25) = (5 * (26-25)) div 2; 

val card6 = (Jack,Spades);
val card7 = (Ace,Spades);
val card8 = (Ace, Clubs);
val card9 = (Ace, Hearts);

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
