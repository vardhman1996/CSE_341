(*Test cases for is_older*)

val iot1 = is_older((1,2,2015),(1,3,2014))
val iot2 = is_older((1,3,2014),(1,2,2015))
val iot3 = is_older((1,3,2014),(1,3,2014))

(*Test cases for number in month*)

val nim1 = number_in_month([(1,2,2014),(1,2,2016),(1,9,2019),(1,2,2011)],2)
val nim2 = number_in_month([(1,2,2015),(1,5,2014)], 7)
val nim3 = number_in_month([], 1)

(*Test cases for number in months*)

val nims1 = number_in_months([(1,2,2014),(1,2,2016),(1,9,2019),(1,2,2011)],[2,9])
val nims2 = number_in_months([(1,2,2015),(1,5,2014)], [1,7,3,5])
val nims3 = number_in_months([], [])
			 
(*Test cases for dates in month*)

val dim1 = dates_in_month([(1,2,2014),(1,2,2016),(1,9,2019),(1,2,2011)],2)
val dim2 = dates_in_month([(1,2,2015),(1,5,2014)], 1)
val dim3 = dates_in_month([], 1)

(*Test cases for dates in months*)

val dims1 = dates_in_months([(1,2,2014),(1,2,2016),(1,9,2019),(1,2,2011)],[2,9])
val dims2 = dates_in_months([(1,2,2015),(1,5,2014)], [1,7,3,5])
val dims3 = dates_in_months([], [])

(*Test cases for get_nth*)

val gn1 = get_nth(["1","2","3","4"], 1)
val gn2 = get_nth(["1","2","3","4"], 4)
val gn3 = get_nth(["1","2","3","4"], 2)

(*Test case for date_to_string*)

val d2s = date_to_string(10,9,2015)

(*Test cases for number_before_reaching_sum*)

val nbrs1 = number_before_reaching_sum(6, [1,2,3,4])
val nbrs2 = number_before_reaching_sum(8, [1,2,3,4])
val nbrs3= number_before_reaching_sum(1, [1,2,3,4])

(*Test cases for what_month*)

val wm1 = what_month(59)
val wm2 = what_month(60)
val wm3 = what_month(2)

(*Test cases for month_range*)

val mr1 = month_range(2,10)
val mr2 = month_range(59,60)

(*Test cases for oldest*)

val o1 = oldest([(1,2,2016),(1,2,1912),(2,1,1913),(2,5,1890),(5,4,1974)])
val o2 = oldest([])

(*Test cases for cumlative_sum*)

val cs1 = cumulative_sum([12,27,13])
val cs2 = cumulative_sum([])

(*Test case for number_in_months_challenge*)

val nimc = number_in_months_challenge([(1,2,2014),(1,2,2016),(1,9,2019),(1,2,2011)],[2,2,9,2,9,5])

(*Test case for dates_in_months_challenge*)

val dimc = dates_in_months_challenge([(1,2,2014),(1,2,2016),(1,9,2019),(1,2,2011)],[2,2,9,2,9,5])

(*Test case for reasonable_date*)

val rd1 = reasonable_date((29,2,2012))
val rd2 = reasonable_date((29,2,2015))
val rd3 = reasonable_date((0,11,2016))
val rd4 = reasonable_date((2,13,2016))
val rd5 = reasonable_date((28,2,2015))
