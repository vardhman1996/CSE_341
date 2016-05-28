(* Vardhman Mehta
   1428251
   mehtav@uw.edu *)

(* Problem 1 *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
  if #3 date1 < #3 date2
  then true
  else if #3 date1 = #3 date2
  then if #2 date1 < #2 date2
       then true
       else if #2 date1 = #2 date2
       then if #1 date1 < #1 date2
	    then true
	    else false
       else false
  else false
	   
(* Problem 2 *)
fun number_in_month (dateList : (int*int*int) list, month : int) =
  if null dateList
  then 0
  else if (#2 (hd(dateList))) = month
  then 1 + number_in_month(tl(dateList), month)
  else 0 + number_in_month(tl(dateList), month)

(* Problem 3 *)
fun number_in_months (dateList : (int*int*int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dateList, hd(months)) + number_in_months(dateList, tl(months))

(* Problem 4 *)						       
fun dates_in_month (dateList : (int*int*int) list, month : int) =
  if null dateList
  then []
  else if #2 (hd(dateList)) = month
  then hd(dateList)::dates_in_month(tl(dateList) , month)
  else dates_in_month(tl(dateList), month)

(* Problem 5 *)
fun dates_in_months (dateList : (int*int*int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dateList, hd(months)) @ dates_in_months(dateList, tl(months))

(* Problem 6 *)
fun get_nth (stringList : string list, n : int) =
  if n = 1
  then hd(stringList)
  else get_nth(tl stringList, n - 1)

val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];

(* Problem 7 *)
fun date_to_string (date : int*int*int) =
  get_nth(months, #2 date) ^ "-" ^ Int.toString(#1 date) ^ "-" ^ Int.toString(#3 date)  

(* Problem 8 *)
fun number_before_reaching_sum (sum : int, numList : int list) =
  if sum - hd(numList) <= 0
  then 0
  else 1 + number_before_reaching_sum(sum - hd(numList), tl(numList))

val monthNum = [31,28,31,30,31,30,31,31,30,31,30,31];

(* Problem 9 *)
fun what_month (day : int) =
  number_before_reaching_sum(day, monthNum) + 1

(* Problem 10 *)
fun month_range (day1 : int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1)::month_range(day1 + 1, day2)

(* Problem 11 *)
fun oldest (dateList : (int*int*int) list) =
  if null dateList
  then NONE
  else
      let
	  fun oldest_nonEmpty (dateList : (int*int*int) list) =
	    if null (tl dateList)
	    then hd dateList
	    else let val tl_ans = oldest_nonEmpty(tl dateList)
		 in
		     if is_older(tl_ans, hd dateList)
		     then tl_ans
		     else hd dateList
		 end
      in
	  SOME (oldest_nonEmpty(dateList))
      end

(* Problem 12 *)
fun cumulative_sum (numList : int list) =
  if null numList
  then []
  else
      let
	  fun sum (curSum : int, numList : int list) =
	    if null numList
	    then []
	    else (curSum + hd numList)::(tl numList)
					    
	  val curSum = sum(hd numList, tl numList);
      in
	  hd numList::cumulative_sum(curSum)
      end


fun checkDup (num : int, numList : int list) =
  if null numList
  then false
  else (num = hd numList) orelse checkDup(num, tl numList)
					  
fun remove_dup (months : int list) =
  if null months
  then []
  else
      let
	  val firstElm = hd months
	  val tailList = tl months
      in
	  if checkDup(firstElm, tailList)
	  then remove_dup(tailList)
	  else firstElm::remove_dup(tailList)
      end
	  
(* Problem 13 *)
fun number_in_months_challenge (dateList : (int*int*int) list, months : int list) =
  number_in_months (dateList, remove_dup(months))

(* Problem 13 *)
fun dates_in_months_challenge (dateList : (int*int*int) list, months : int list) =
  dates_in_months(dateList, remove_dup(months))

val monthsWithLeap = [31,29,31,30,31,30,31,31,30,31,30,31];

(* Problem 14 *)
fun reasonable_date (date : int*int*int) =
  if #3 date <= 0
  then false
  else
      if #2 date < 1 orelse #2 date > 12
      then false
      else
	  let
	      val year = #3 date
	      val isLeap = (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0)
	      val leap = if isLeap
			 then monthsWithLeap
			 else monthNum
	      fun get_month (months : int list, num : int) =
		if num = 1
		then hd months
		else get_month(tl months, num - 1)
	  in
	      if (#1 date < 1) orelse (#1 date) > get_month(leap, #2 date)
	      then false
	      else true
	  end
