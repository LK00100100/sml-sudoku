(* LK00100100 with Professor Fodor's CSE 307 *)
(* solves a 6-number sudoku board *)
(* note: the solver takes a few minutes *)
(* note: everything is 1-indexed *)

(* gets the length of a list*)
fun length([]) = 0
| length([x]) = 1
| length(x::xs) = 1 + length(xs);

(* counts the empty spaces in a row *)
fun emptyRowSpaces(row:int list) =
if(length(row) = 0) then 0
else if(hd(row) = ~1) then 1 + emptyRowSpaces(tl(row))
else emptyRowSpaces(tl(row));

(* counts the empty spaces in an int_list_list *)
fun countEmptyBoardSpaces(board:int list list) =
if(length(board) = 0) then 0
else emptyRowSpaces(hd(board)) + countEmptyBoardSpaces(tl(board));

(* returns a list, row, from a list list *)
fun getBoardRow(board:int list list, row:int) =
if(row = 1) then hd(board)
else getBoardRow(tl(board), row - 1);

(* returns an int from the list *)
fun getBoardCol(row:int list, col:int) =
if(col = 1) then hd(row)
else getBoardCol(tl(row), col - 1);

(* returns board[row][col] = int *)
fun getBoardSpace(board:int list list, row:int, col:int) =
getBoardCol(getBoardRow(board, row), col);

(* set's x in the row's col *)
(* list, int, int -> list *)
fun setRowCol(row:int list, col:int, x:int) =
if(col = 1) then
	[x] @ tl(row)
else
	[hd(row)] @ setRowCol(tl(row), col - 1, x);
	
(* board[row][col] = x *)
fun setBoardSpace(board:int list list, row:int, col:int, x:int) =
if(length(board) = 0) then board
else if (row = 1) then 
	[setRowCol(hd(board), col, x)] @ tl(board)
else
	[hd(board)] @ setBoardSpace(tl(board), row - 1, col, x);

(* does this row contain X? *)
fun containXRow(row:int list, x:int) =
if(length(row) = 0) then false
else if(hd(row) = x) then true
else containXRow(tl(row), x);

(* does this row contain X within the first N elements?*)
fun containXRowN(row:int list, x:int, n:int) =
if(length(row) = 0 orelse n = 0) then false
else if(hd(row) = x) then true
else containXRowN(tl(row), x, n - 1);

(* does this board contain x in row n? *)
fun boardContainXRowN(board:int list list, row:int, x:int) =
if(row = 1) then
	containXRow(hd(board), x)
else
	boardContainXRowN(tl(board), row - 1, x);

(* does the column, col, contain X? *)
fun containXCol(board:int list list, col:int, x:int) =
if(length(board) = 0) then false
else if(getBoardCol(hd(board), col) = x) then true
else containXCol(tl(board), col, x);

(* removes the first x elements from the list L *)
fun removeXFirstList(L:int list, x:int) =
if(x = 0) then L
else removeXFirstList(tl(L), x - 1);

(* put in a row and get the section's row *)
fun sectionRow(row:int) =
if row <= 2 then 1
else if row <= 4 then 3
else 5;
	
(* put in a col and get the section's col *)
fun sectionCol(col:int) =
if col <= 3 then 1
else 4;

(* does the section, starting at row/col, have x in it? *)
fun sectionContainX(board:int list list, row:int, col:int, x:int) =
if(row = 1) then
	(* does this section's two row have this x? *)
	if(containXRowN(removeXFirstList(hd(board), col - 1), x, 3)) orelse 
		(containXRowN(removeXFirstList(hd(tl(board)), col - 1), x, 3)) then true
	else false
(* reduce the rows until you are at the first row *)
else sectionContainX(tl(board), row - 1, col, x);

(* is this place on the board a valid spot to place x? *)
fun isInvalidSpot(board:int list list, row:int, col:int, x:int) =
	boardContainXRowN(board, row, x) orelse containXCol(board, col, x) orelse sectionContainX(board, sectionRow(row), sectionCol(col), x);
	
fun isBoardSpaceEmpty(board:int list list, row:int, col:int) =
if getBoardSpace(board, row, col) = ~1 then	true
else false;

(*
brute force magic
board - current Board state
row, col - space being examined
emptyCount - empty spaces
num - number to try to place

x - the number you are trying to place in this spot
returns the board with the solution (or one without)

returns board
*)
fun solveBoard(board:int list list, row:int, col:int, emptyCount:int, x:int) =
if(emptyCount = 0) then board (* solution found *)
else
	if(row > 6) then board (* no more board left *)
	else if(x > 6) then board (* we've tried all six numbers in this spot*)
	else
		(* we got an empty space *)
		if isBoardSpaceEmpty(board, row, col) = true then				
			(* if invalid, don't place *)
			if isInvalidSpot(board, row, col, x) = true then 
				(* try the next target number *)
				solveBoard(board, row, col, emptyCount, x + 1)
			(* place a number. this should have a solution *)
			else if(col = 6) then (* go to next row *)
				if countEmptyBoardSpaces(solveBoard(setBoardSpace(board, row, col, x), row + 1, 1, emptyCount - 1, 1)) = 0 then
					solveBoard(setBoardSpace(board, row, col, x), row + 1, 1, emptyCount - 1, 1)
				(* no solution, try the next number *)
				else
					solveBoard(board, row, col, emptyCount, x + 1)
			(* place a number*)
			else
				if countEmptyBoardSpaces(solveBoard(setBoardSpace(board, row, col, x), row, col + 1, emptyCount - 1, 1)) = 0 then
					solveBoard(setBoardSpace(board, row, col, x), row, col + 1, emptyCount - 1, 1)
				else
					solveBoard(board, row, col, emptyCount, x + 1)
		(*get next empty spot*)
		else
			if(col = 6) then
				solveBoard(board, row + 1, 1, emptyCount, 1)
			else
				solveBoard(board, row, col + 1, emptyCount, 1);

(* the main function *)
fun solve(x:int list list) =
solveBoard(x, 1, 1, countEmptyBoardSpaces(x), 1);

solve([
[2, 1, ~1, ~1, 4, 3],
[~1, ~1, ~1, ~1, ~1, ~1],
[~1, ~1, 6, 2, ~1, ~1],
[~1, ~1, 3, 4, ~1, ~1],
[~1, ~1, ~1, ~1, ~1, ~1],
[3, 4, ~1, ~1, 5, 6]]);

solve([
[~1, ~1, ~1, ~1, ~1, ~1],
[2, ~1, 6, 4, ~1, 3],
[1, ~1, ~1, ~1, ~1, 6],
[5, ~1, ~1, ~1, ~1, 2],
[3, ~1, 2, 5, ~1, 4],
[~1, ~1, ~1, ~1, ~1, ~1]]);

solve([
[~1, ~1, 2, 6, ~1, ~1],
[~1, 4, ~1, ~1, 3, ~1],
[1, ~1, ~1, ~1, ~1, 4],
[2, ~1, ~1, ~1, ~1, 1],
[~1, 6, ~1, ~1, 2, ~1],
[~1, ~1, 3, 4, ~1, ~1]]);

solve([
[2, ~1, ~1, ~1, ~1, 4],
[6, ~1, ~1, ~1, ~1, 3],
[~1, ~1, 1, 3, ~1, ~1],
[~1, ~1, 6, 4, ~1, ~1],
[4, ~1, ~1, ~1, ~1, 5],
[1, ~1, ~1, ~1, ~1, 2]]);
