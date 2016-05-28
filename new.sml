val e = 2;

val pair = ((3,true),(4,"asd"));

val l = [1,2,3,4,5];

fun pow (x : int, y : int) =
  if y = 0
  then 1
  else x * pow(x, y-1)

val ans = pow(2,2);

val fistPair = #1 pair;

val newList = 0::l;

val tailList = tl newList;

fun sum_list (x : int list) =
  if null x
  then 0
  else hd(x) + sum_list(tl(x))

val sum = sum_list(newList);

val sum = 0;
