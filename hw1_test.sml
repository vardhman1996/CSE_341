use "hw1.sml";

(* testing is_older *)

val date1 = (1,2,2016);
val date2 = (28,5,2016);
val date3 = (29,2,2016);
val date4 = (15,9,2017);
val dateList = [date1, date2, date3, date4];



val is_olderTest = [is_older(date1, date2) = true, 
		    is_older(date2, date1) = false, 
		    is_older(date1, date1) = false, 
		    is_older(date3, date1) = false,
		    is_older(date1, date3) = true,
		    is_older(date1, date4) = true,
		    is_older(date4, date1) = false]

val number_in_month_test = [number_in_month(dateList, 2) = 2,
							number_in_month(dateList, 5) = 1,
							number_in_month(dateList, 9) = 1,
							number_in_month(dateList, 12) = 0]

val monthList = [2,5,9];

val number_in_months_test = [number_in_months(dateList, monthList) = 4,
							 number_in_months(dateList, tl monthList) = 2,
							 number_in_months(dateList, []) = 0]

val dates_in_month_test = [dates_in_month(dateList, 2) = [date1, date3],
						   dates_in_month(dateList, 5) = [date2],
						   dates_in_month(dateList, 1) = []]

val dates_in_months_test = [dates_in_months(dateList, monthList) = [date1, date3, date2, date4],
							dates_in_months(dateList, tl monthList) = [date2, date4],
							dates_in_months(dateList, [1,3,6]) = []]

val stringList = ["a", "b", "c", "d"];

val get_nth_test = [get_nth(stringList, 1) = "a",
					get_nth(stringList, 2) = "b"]

val date_to_string_test = [date_to_string(date1) = "February-1-2016",
						   date_to_string(date4) = "September-15-2017",
						   date_to_string(date2) = "May-28-2016",
						   date_to_string(date3) = "February-29-2016"]

val numList = [1, 7, 12, 18, 19];

val number_before_reaching_sum_test = [number_before_reaching_sum(1, numList) = 0,
									   number_before_reaching_sum(6, numList) = 1,
									   number_before_reaching_sum(10, numList) = 2,
									   number_before_reaching_sum(55, numList) = 4]

val what_month_test = [what_month(31) = 1,
					   what_month(59) = 2,
					   what_month(150) = 5,
					   what_month(365) = 12]


val month_range_test = [month_range(30, 33) = [1,1,2,2],
						month_range(150, 150) = [5],
						month_range(100, 99) = []]

val oldest_test = [oldest(dateList) = SOME(date1),
				   oldest([]) = NONE,
				   oldest([date1]) = SOME(date1)]

val cumulative_sum_test = [cumulative_sum([1,2,3]) = [1,3,6],
						   cumulative_sum([1]) = [1],
						   cumulative_sum([]) = []]

val number_in_months_challenge_test = [number_in_months_challenge(dateList, monthList) = number_in_months(dateList, monthList),
									   number_in_months_challenge(dateList, tl monthList) = number_in_months(dateList, tl monthList),
									   number_in_months_challenge(dateList, [1,3,6]) = number_in_months(dateList, [1,3,6]),
									   number_in_months_challenge(dateList, monthList @ monthList) = number_in_months(dateList, monthList)]

val dates_in_months_challenge_test = [dates_in_months_challenge(dateList, monthList) = dates_in_months(dateList, monthList),
									  dates_in_months_challenge(dateList, tl monthList) = dates_in_months(dateList, tl monthList),
									  dates_in_months_challenge(dateList, [1,3,6]) = dates_in_months(dateList, [1,3,6]),
									  dates_in_months_challenge(dateList, monthList @ monthList) = dates_in_months(dateList, monthList)]

val reasonable_date_test = [reasonable_date(date1) = true,
							reasonable_date(date2) = true,
							reasonable_date(date3) = true,
							reasonable_date(date4) = true,
							reasonable_date((~1, 1, 2015)) = false,
							reasonable_date((29, 2, 2015)) = false,
							reasonable_date((29, 2, 2016)) = true]

