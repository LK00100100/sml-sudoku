(* sample functions*)
(* By LK00100100 in 2015*)

(* get the last element in a list *)
fun last([]) = 0
| last([x]) = x
| last(x::xs) = last(xs);

last([]);
last([1]);
last([1,2,3]);

(* get the k-th element in a list *)
fun kth([], K:int) = 0
| kth(x::xs, 1) = x
| kth(x::xs, K) = kth(xs, K-1);

kth([], 3);
kth([1], 3);
kth([1,2,3], 3);

(* reverse a list *)
fun reverse([]) = []
| reverse(x::xs) = reverse(xs) @ [x];

reverse([]);
reverse([1]);
reverse([1,2,3]);

(* get the length of a list *)
fun length(L) =
if (L = nil) then 0
else 1 + length(tl(L));

length([]);
length([1]);
length([1,2,3]);

(* remove the last element of a list *)
fun removeLast([]) = []
| removeLast([x]) = []
| removeLast(x::xs) = [x] @ removeLast(xs);

removeLast([]);
removeLast([1]);
removeLast([1,2,3]);

(* return 1 if a list is a palindrome. 0 if not *)
fun palindrome(L) =
if(L = nil) then 1
else if (length(L) = 1) then 1
else if (hd(L) = last(L)) then palindrome(removeLast(tl(L)))
else 0;

palindrome([]);
palindrome([1]);
palindrome([1,2,3]);
palindrome([3,2,3]);
palindrome([3,2,2,3]);

(* turn a 2d list in a 1d list *)
fun flattenList(L) =
if (length(L) = 0) then []
else if (hd(L) = []) then flattenList(tl(L))
else [hd(hd(L))] @ flattenList([tl(hd(L))] @ tl(L));

flattenList([[]]);
flattenList([[1,1]]);
flattenList([[1,1], [2,2], [3,3]]);

(* removeConsec() helper *)
fun removeConsec2(L, N:int) =
if(length(L) = 0) then []
else if (hd(L) = N) then removeConsec2(tl(L), N)
else [hd(L)] @ removeConsec2(tl(L), hd(L));

(* consolidate consecutive elements *)
(* [1, 1, 2, 2] = [1, 2] *)
fun removeConsec(L) =
if(length(L) = 0) then []
else if(length(L) = 1) then L
else [hd(L)] @ removeConsec2(tl(L), hd(L));

removeConsec([]);
removeConsec([1]);
removeConsec([1,1,2,2,3,3,1,1,2,2,3,3,3,3]);

(* pack duplicates helper *)
fun packDuplicates2(L, X:int) =
if(length(hd(tl(L))) = 0) then [hd(L)]
else if(hd(hd(tl(L))) = X) then packDuplicates2([X::hd(L), tl(hd(tl(L)))], X)
else hd(L)::packDuplicates2([[hd(hd(tl(L)))], tl(hd(tl(L)))], hd(hd(tl(L))));

(* inputs a 1d list. it bundles like numbers into its own list *)
(* [1,1,2,2,3] = [[1,1],[2,2],[3]]*)
fun packDuplicates(L) =
if(length(L) = 0) then nil
else packDuplicates2([[hd(L)], tl(L)], hd(L));

packDuplicates([]);
packDuplicates([1]);
packDuplicates([1,1,1,1,2,2,2,3,3,3,1,1,1,1,2,2,2,3,3,3]);

(* encode() helper *)
fun encode2(L) = 
if(length(L) = 0) then []
else [length(hd(L)), hd(hd(L))] :: encode2(tl(L));

(* compresses a list. [quantity of number, number] *)
(* [1,1,1] => [[3,1]] there are three 1's *)
(* [1,1,1,1,2,3,3] => [[4,1],[1,2],[2,3]] *)
fun encode(L) =
if(length(L) = 0) then nil
else encode2(packDuplicates(L));

encode([]);
encode([1]);
encode([1,1,1,1,2,3,3,1,1,4,5,5,5,5]);

(* takes two numbers and transforms it into a list *)
(* buildList(amount of number, number)) *)
(* buildList(3,6) => [6,6,6]. three 6's *)
fun buildList(A:int, N:int) =
if (A = 0) then nil
else if (A = 1) then [N]
else N::buildList(A - 1, N);

buildList(0, 0);
buildList(3, 6);

(* Does opposite of encode() above *)
fun decode(L) =
if(length(L) = 0) then nil
else buildList(hd(hd(L)), hd(tl(hd(L)))) @ decode(tl(L));

decode([]);
decode([[4,1]]);
decode([[4,1],[1,2],[2,3],[2,1],[1,4],[4,5]]);

(* generates a list from a range of numbers *)
(* range(left, right) *)
(* range(4,7) => [4,5,6,7] *)
fun range(L:int, R:int) =
if(L = R) then [L]
else [L] @ range(L+1, R);

range(0,0);
range(4,9);
