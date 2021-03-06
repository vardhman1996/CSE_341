use "hw3.sml";

val list1 = ["abc", "Abc", "hello", "Hiiii"]

val only_lowercase_test = [only_lowercase(list1) = ["abc", "hello"],
			   only_lowercase(tl list1) = ["hello"],
			   only_lowercase(["Abc"]) = []]

val longest_string1_test = [longest_string1(list1) = "hello",
			    longest_string1([]) = ""]

val longest_string2_test = [longest_string2(list1) = "Hiiii",
			    longest_string2([]) = ""]

val longest_string3_test = [longest_string3 list1 = "hello",
			    longest_string3 [] = ""]

val longest_string4_test = [longest_string4 list1 = "Hiiii",
			    longest_string4 [] = ""]

val longest_lowercase_test = [longest_lowercase list1 = "hello",
			      longest_lowercase [] = ""]

val rev_string_test = [rev_string "hi" = "IH",
		       rev_string "" = "",
		       rev_string "abcd" = "DCBA"]
			  
val first_answer_test = (first_answer (fn (x) => if x > 5 then SOME x else NONE) [1,2,3,4,5,6,7,8,9]) = 6

val all_answers_test = (all_answers (fn (x) => if x > 5 then SOME [x] else NONE) [6,7,8,9]) = SOME [6,7,8,9]

val all_answers_test2 = (all_answers (fn (x) => if x > 5 then SOME [x] else NONE) []) = SOME []

val count_wildcards_test = [count_wildcards (TupleP[WildcardP, ConstructorP("foo", WildcardP), UnitP]) = 2,
			    count_wildcards UnitP = 0]

val count_wild_and_variable_lengths_test = [count_wild_and_variable_lengths (TupleP[WildcardP, ConstructorP("foo", WildcardP), VariableP "bar"]) = 5, count_wild_and_variable_lengths (VariableP "ABC") = 3]

val count_a_var_test = [count_a_var ("abc" , TupleP[VariableP "abc", ConstructorP("foo", WildcardP), UnitP]) = 1,
		        count_a_var ("abc", VariableP "v") = 0]

val check_pat_test = [check_pat (TupleP[VariableP "x", VariableP "y", UnitP]) = true,
		      check_pat (TupleP[VariableP "x", VariableP "x", UnitP]) = false]

val match_test = [match (Constant 0, TupleP[ConstantP 0]) = NONE,
		  match (Constant 0, WildcardP) = SOME [],
		  match (Constant 0, ConstantP 0) = SOME [],
		  match (Constant 0, VariableP "a") = SOME[("a", Constant 0)]]

val first_match_test = [first_match (Constant 0) [ConstantP 0] = SOME [],
			first_match (Constant 0) [VariableP "a", ConstantP 0, ConstantP 2] = SOME [("a",Constant 0)]]
