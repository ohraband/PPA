r() :- reconsult("sudoku.pl").

%fill_sudoku(+SudokuInput -SolvedSudoku)
fill_sudoku(Sudoku,NewSudoku) :- list_len(Sudoku, Length), solve_sudoku(Sudoku, 0, Length, NewSudoku).

%solve_sudoku(+SudokuInput, +RowIndex, +SudokuRowLength, -SolvedSudoku)
solve_sudoku(Sudoku, Length, Length, Sudoku).
solve_sudoku(Sudoku, RowIndex, Length, X) :- SquareSize is round(sqrt(Length)), NRowIndex is RowIndex + 1, NRowIndex =< Length, nth_elem(Sudoku, RowIndex, Row), fill_row2(Row, NewRow, Sudoku), replace_nth(Sudoku, RowIndex, NewRow, NewSudoku),
						 SquaresRow is ( (RowIndex // SquareSize) * SquareSize),  check_all_squares_in_row(SquaresRow, NewSudoku, 0, Length, SquareSize),
						 solve_sudoku(NewSudoku, NRowIndex, Length, X).

%fill_row2(+RowInput, -FilledRow, +SudokuInput)
fill_row2(Row, NewRow, Sudoku) :- list_len(Row, Length), fill_row(Row, 0, Length, Row, NewRow, Sudoku).


%fill_row(+RowInput, +RowIndex, +RowLength, +RowInput, -FilledRow, +SudokuInput)
fill_row(Row, Length, Length, _, Row, _).


fill_row(Row, Pos, Length, [H|T], X, Sudoku) :- H = 0, row_fill_number(Pos, 0, Length, Row, NewRow), get_col(Sudoku, Pos, Column), 
							nth_elem(NewRow, Pos, NewElem), not_contains(Column, NewElem),
						Pos2 is Pos + 1, Pos2 =< Length, fill_row(NewRow, Pos2, Length, T, X, Sudoku).

fill_row(Row, Pos, Length, [H|T], X, Sudoku) :-  H \= 0, Pos2 is Pos + 1, Pos2 =< Length, fill_row(Row, Pos2, Length, T, X, Sudoku).




%row_fill_number(+RowIndex, +CurrentNumber, +MaximumNumber, +RowInput, -RowInputWithRowIndexFilled)
row_fill_number(Pos, Elem, Max, Row, NewRow) :- not_contains(Row, Elem), replace_nth(Row, Pos, Elem, NewRow); Elem2 is Elem +1 , Elem2 =< Max, row_fill_number(Pos, Elem2, Max, Row, NewRow).

%get_firstn_elem(+N, +StartingIndex, +Lst, -Lst2)
get_firstn_elem(0,_,_,[]).
get_firstn_elem(N, Pos, Row, [Elem|X]) :- N1 is N - 1, N1 >= 0 , N2 is N1 + Pos, nth_elem(Row, N2, Elem), get_firstn_elem(N1,Pos,Row, X).


%get_square(+SquareSize, +SquareSize, +ColInex, +Sudoku, +RowIndex, -LstOfLstOfRows)
get_square(0,_,_,_,_,[]).
get_square(N, Size, Pos,  Sudoku, RowIndex, [Y|X]) :- N1 is N -1, N1 >= 0, NRowIndex is RowIndex +1,  nth_elem(Sudoku, RowIndex, Row), get_firstn_elem(Size, Pos, Row, Y),
								get_square(N1, Size, Pos, Sudoku, NRowIndex, X).

%square_to_list(+SquareSize, +Sudoku, +RowIndex, +ColIndex, -LstSquare)
square_to_list(Size, Sudoku, RowIndex, ColIndex, Square) :- get_square(Size, Size, ColIndex,  Sudoku, RowIndex, Res), flatten_list(Res, Square).

%check_square(+LstSquare)
check_square([]) :- !.
check_square([H|T]) :- H = 0, check_square(T), !.
check_square([H|T]) :- not_contains(T, H), check_square(T), !.

%check_all_squares_in_row(+RowIndex, +Sudoku, +ColIndex, +SudokuRowLength, +SquareSize)
check_all_squares_in_row(_, _, Size, Size, _).
check_all_squares_in_row(RowIndex, Sudoku, ColIndex, Size, SquareSize) :- square_to_list(SquareSize, Sudoku, RowIndex, ColIndex, Square), check_square(Square), ColIndex1 is ColIndex + SquareSize, ColIndex1 =< Size,
										check_all_squares_in_row(RowIndex, Sudoku, ColIndex1, Size, SquareSize).


%replace_nth(+Lst, +Pos, +Elem, -Lst2)
replace_nth([_|T],0, New, [New|T]).
replace_nth([H|T],N, New, [H|Res]) :- Q is N-1, Q >= 0, replace_nth(T,Q,New,Res).

%--------------------------------------------------------------------------------------------------------------------------------------------
% Následujúce funkcie sú, aj vzhľadom na ich jednoduchosť, implementované veľmi podobne ako na proseminári.
% https://gitlab.fit.cvut.cz/BI-PPA/bi-ppa/blob/master/tutorials/solutions/prolog-2.pl

% get_col(+Sudoku, +N, -Res)
get_col([], _, []).
get_col([Row|T], N, [Elem|Res]) :- nth_elem(Row, N, Elem), get_col(T, N, Res).

% list_len(+Lst, -Res).
list_len([], 0).
list_len([_|T], Res) :- list_len(T, Res1), Res is Res1 + 1.

% not_contains(+Lst, +Elem).
not_contains([], _).
not_contains([H|_], H) :- fail.
not_contains([H|T], E) :- H \= E, not_contains(T, E).

% nth_elem(+Lst, +N, -Elem).
nth_elem([H|_], 0, H) :- !.
nth_elem([_|T], N, Elem) :- N1 is N-1, nth_elem(T, N1, Elem).

% append_list(+Lst1, +Lst2, -Res).
append_list([], Lst2, Lst2).
append_list([H|T], Lst2, [H|Res]) :- append_list(T, Lst2, Res).


% flatten_list(+Lst, -Res).
flatten_list([], []).
flatten_list([H|T], Res) :- is_list(H), flatten_list(H, Res1), flatten_list(T, Res2), append_list(Res1, Res2, Res), !.
flatten_list([H|T], [H|Res]) :- flatten_list(T, Res).

% elem_count(+Elem, +Lst, -Cnt)
elem_count(_, [], 0) :- !.
elem_count(Elem, [H|T], Cnt)  :- Elem \= H, elem_count(Elem, T, Cnt), !.
elem_count(Elem, [Elem|T], Cnt1) :- elem_count(Elem, T, Cnt), Cnt1 is Cnt + 1.

%------------------------------------------------------------------------------------------------------------------------------------------


easysudoku :- Sudoku = 
       [[1, 0, 0,    0, 2, 0,    7, 0, 6],
        [9, 8, 0,    4, 7, 0,    0, 0, 2],
        [4, 7, 0,    0, 1, 0,    8, 9, 0],

        [5, 6, 0,    2, 9, 0,    3, 7, 8],
        [7, 9, 0,    0, 6, 3,    4, 0, 0],
        [0, 4, 0,    0, 0, 0,    0, 6, 0],

        [8, 1, 0,    7, 0, 2,    6, 0, 3],
        [6, 0, 0,    9, 0, 8,    0, 0, 0],
        [3, 2, 7,    0, 5, 1,    0, 8, 4]],
	fill_sudoku(Sudoku,X), 
	X  = 
        [[1, 3, 5,    8, 2, 9,    7, 4, 6],
        [9, 8, 6,    4, 7, 5,    1, 3, 2],
        [4, 7, 2,    3, 1, 6,    8, 9, 5],

        [5, 6, 1,    2, 9, 4,    3, 7, 8],
        [7, 9, 8,    5, 6, 3,    4, 2, 1],
        [2, 4, 3,    1, 8, 7,    5, 6, 9],

        [8, 1, 9,    7, 4, 2,    6, 5, 3],
        [6, 5, 4,    9, 3, 8,    2, 1, 7],
        [3, 2, 7,    6, 5, 1,    9, 8, 4]]
.

hardsudoku :- Sudoku = 
       [[8, 0, 0,    0, 0, 0,    0, 0, 0],
        [0, 0, 3,    6, 0, 0,    0, 0, 0],
        [0, 7, 0,    0, 9, 0,    2, 0, 0],

        [0, 5, 0,    0, 0, 7,    0, 0, 0],
        [0, 0, 0,    0, 4, 5,    7, 0, 0],
        [0, 0, 0,    1, 0, 0,    0, 3, 0],

        [0, 0, 1,    0, 0, 0,    0, 6, 8],
        [0, 0, 8,    5, 0, 0,    0, 1, 0],
        [0, 9, 0,    0, 0, 0,    4, 0, 0]],
	fill_sudoku(Sudoku,X),
	X = 
       [[8, 1, 2,    7, 5, 3,    6, 4, 9],
        [9, 4, 3,    6, 8, 2,    1, 7, 5],
        [6, 7, 5,    4, 9, 1,    2, 8, 3],

        [1, 5, 4,    2, 3, 7,    8, 9, 6],
        [3, 6, 9,    8, 4, 5,    7, 2, 1],
        [2, 8, 7,    1, 6, 9,    5, 3, 4],

        [5, 2, 1,    9, 7, 4,    3, 6, 8],
        [4, 3, 8,    5, 2, 6,    9, 1, 7],
        [7, 9, 6,    3, 1, 8,    4, 5, 2]].


smallsudoku :- Sudoku =
       [[1,0,	2,4],
	[4,2,	0,0],
	
	[2,4,	1,3],
	[0,0,	4,2]],
	fill_sudoku(Sudoku,X),
	X = 
       [[1,3,	2,4],
	[4,2,	3,1],
	
	[2,4,	1,3],
	[3,1,	4,2]]
	.
	


test_get_square :- Sudoku =  
       [[1, 0, 0,    0, 2, 0,    7, 0, 6],
        [9, 8, 0,    4, 7, 0,    0, 0, 2],
        [4, 7, 0,    0, 1, 0,    8, 9, 0],

        [5, 6, 0,    2, 9, 0,    3, 7, 8],
        [7, 9, 0,    0, 6, 3,    4, 0, 0],
        [0, 4, 0,    0, 0, 0,    0, 6, 0],

        [8, 1, 0,    7, 0, 2,    6, 0, 3],
        [6, 0, 0,    9, 0, 8,    0, 0, 0],
        [3, 2, 7,    0, 5, 1,    0, 8, 4]],

%                    ↑↑↑↑↑↑↑↑	
%Numbers in each row are reversed, but values should match.
%square_to_list(+SquareSize, +Sudoku, +RowIndex, +ColIndex, -LstSquare)
	square_to_list(3,Sudoku,6,3,Square),
	Square = [2,0,7,8,0,9,1,5,0]
	.	
	

test_fill_row :- Row =  
       [1, 0, 0,    0, 2, 0,    7, 0, 6],

	Sudoku = 
       [[1, 0, 0,    0, 2, 0,    7, 0, 6],
        [9, 8, 0,    4, 7, 0,    0, 0, 2],
        [4, 7, 0,    0, 1, 0,    8, 9, 0],

        [5, 6, 0,    2, 9, 0,    3, 7, 8],
        [7, 9, 0,    0, 6, 3,    4, 0, 0],
        [0, 4, 0,    0, 0, 0,    0, 6, 0],

        [8, 1, 0,    7, 0, 2,    6, 0, 3],
        [6, 0, 0,    9, 0, 8,    0, 0, 0],
        [3, 2, 7,    0, 5, 1,    0, 8, 4]], 

%Checks row and col, not square.
%fill_row(+RowInput, +RowIndex, +RowLength, +RowInput, -FilledRow, +SudokuInput)
	fill_row(Row, 0, 9, Row, Res, Sudoku),
	Res = [1,3,4,8,2,9,7,5,6].

	
       



test_check_squares :- Sudoku =  
       [[1, 3, 5,    8, 2, 9,    7, 4, 6],
        [9, 8, 6,    4, 7, 5,    1, 3, 2],
        [4, 7, 2,    3, 1, 6,    8, 9, 5],

        [5, 6, 1,    2, 9, 4,    3, 7, 8],
        [7, 9, 8,    5, 6, 3,    4, 2, 1],
        [2, 4, 3,    1, 8, 7,    5, 6, 9],

        [8, 1, 9,    7, 4, 2,    6, 5, 3],
        [6, 5, 4,    9, 3, 8,    2, 1, 7],
        [3, 2, 7,    6, 5, 1,    9, 8, 4]],


%check_all_squares_in_row(+RowIndex, +Sudoku, +ColIndex, +SudokuRowLength, +SquareSize)
	check_all_squares_in_row(3,Sudoku,0,9,3),
	check_all_squares_in_row(3,Sudoku,0,9,3),
	check_all_squares_in_row(6,Sudoku,0,9,3)
	.	


test_firstn_elem :- 	
	Row = [1,3,4,8,2,9,7,5,6],
%get_firstn_elem(+N, +StartingIndex, +Lst, -Lst2)
	get_firstn_elem(3,2,Row,Res),
%Again, the elements should be reversed.
	Res = [2,8,4].


test_replace :- 
	Row = [1,3,4,8,2,9,7,5,6],
%replace_nth(+Lst, +Pos, +Elem, -Lst2)
	replace_nth(Row, 2, 7, Res),
	Res = [1,3,7,8,2,9,7,5,6].
	



