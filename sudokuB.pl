/* 

   [REFDACTED]

   Below is a solution to Sudoku Puzzle
   can be found in SWI-Prolog web site. It is fun
   to see how it works for a 9 x 9 sudoku puzzle
   with 3 x 3 mini-blocks. If you have not played
   Sudoku before, please try mini-sudoku first:
      https://www.mathinenglish.com/puzzlessudoku.php

   There are variations of the suduko puzzle
   with different main grid sizes and mini-block sizes.

   For example, junior-sudoku is based on
       4 x 4 grid with 2 x 2 mini-blocks

   Another example is mini-sudoku that is based on
       6 x 6 grid with 3 x 2 mini-blocks

   Task 1 (80%)
   ======
   Generalize your sudoku solution generator using
   a new predicate gen_suduko below which supports
   different variations of sudoku puzzles, based on
   grid and mini-block sizes.

   gen_sudoku(Rows,N,B_R,B_C)
      N - size of entire block of N x N
      B_R - mini-block row size
      B_C - mini-block column size

   We can add the following constraints:
         N #>3, B_R >1, B_C>1, N #= B_R * B_C
   To restrict ourselves to regular-shaped sudokus that
   that are easier for humans to follow.
   
   The output for gen_sudoku will be made using maplist(portray_clause, Rows)
   in the query predicate.

   Task 2 (20%)
   ======
   Design a problem suduko generator, called
           find_puzzle_sudoku(Rows,S,N,M,B_R,B_C)
   that would generate a random sudoku puzzle of grid size S x S,
   mini-blocks B_R x B_C and which has from N to M known number of values. 

   Your solution may make use of random generator predicate
      random(+L:int, +U:int, -R:int)

   It should start with a random puzzle (whose numbers
   are well-spaced out) with N known numbers.
   If this did not return a unique solution, you could
   add one more (random) number to this incomplete puzzle,
   You can progressively do that until it hits M known numbers.

   If no unique puzzle is found with M known numbers, you can
   exit with a false outcome.

   PS You may use a predicate  aggregate_all(count, pred(X), Count). to count number of solutions.
   See https://stackoverflow.com/questions/6060268/prolog-count-the-number-of-times-a-predicate-is-true

   Due to the use of randomization, kindly note that the solution you get
   from this predicate is non-deterministic. You should try to
   think of some solutions that would give you the best possible outcome
   with smallest number of random values used,
   but without sacrificing too much on the time taken for your puzzle
   generator to terminate. If appropriate, you may repeat some of the 
   randomization processes but bearing in mind a trade-off between a 
   better solution versus time-out.
   
   We shall have a mini-competition to see who has the best find_puzzle_sudoku code. 
   For this mini-competition, the winner is one who can use the smallest number of
   known values used, followed by time taken. The competition is 
   just for fun and to encourage you to try your best.
   
*/

:- use_module(library(clpfd)).

problem(1, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,3,_,8,5],
            [_,_,1,_,2,_,_,_,_],
            [_,_,_,5,_,7,_,_,_],
            [_,_,4,_,_,_,1,_,_],
            [_,9,_,_,_,_,_,_,_],
            [5,_,_,_,_,_,_,7,3],
            [_,_,2,_,1,_,_,_,_],
            [_,_,_,_,4,_,_,_,9]]).

problem(2, [[3,_,_,8,_,1,_,_,2],
            [2,_,1,_,3,_,6,_,4],
            [_,_,_,2,_,4,_,_,_],
            [8,_,9,_,_,_,1,_,6],
            [_,6,_,_,_,_,_,5,_],
            [7,_,2,_,_,_,4,_,9],
            [_,_,_,5,_,9,_,_,_],
            [9,_,4,_,8,_,7,_,5],
            [6,_,_,1,_,7,_,_,3]]).

mini_suduko(1,[[_,_,6,_,4,_],
               [_,_,_,_,6,_],
               [_,_,_,5,_,3],
               [3,_,_,_,_,_],
               [_,1,_,_,_,_],
               [_,5,_,_,4,_]]).

junior_suduko(1,[[_,4,_,1],
                 [3,_,4,_],
                 [1,_,_,4],
                 [_,2,1,_]]).

mini_sudoku(Rows) :- gen_sudoku(Rows,6,2,3).
junior_sudoku(Rows) :- gen_sudoku(Rows,4,2,2).
new_sudoku(Rows) :- gen_sudoku(Rows,9,3,3).

/* -------------------- TASK 1 --------------------- */

/* Generalized Sudoku Solver 
  N - size of entire block of N x N
  B_R - mini-block column size
  B_C - mini-block column size
*/
gen_sudoku(Rows,N,B_R,B_C):- 
        N #>1, B_R #> 1, B_C #> 1, N #= B_R * B_C, 
        length(Rows, N), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..N,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns), 
        part(B_R, Rows, Partitions),  
        maplist(valid_blocks(B_C), Partitions).

/* helper functions */
valid_blocks(_, [X|_]) :- X = [], !. 
valid_blocks(B_C, Partition) :- 
    take_firstN_fromEach(B_C, Partition, Grid, OtherGrids), 
    flatten(Grid, FlatGrid), 
    all_distinct(FlatGrid), 
    valid_blocks(B_C, OtherGrids).

/* Result = first N elements of Src List, Rest is Suffix. */
take_firstN(0, Src, [], Src) :- !.
take_firstN(N, [H|T], [H|P], Suffix) :- 
        N #> 0, M #= N-1, 
        take_firstN(M, T, P, Suffix). 

/* Partitions our matrix into groups with N rows per group */ 
part(_, [], []) :- !.
part(N, Rows, [Group | RestGroups]) :- 
        take_firstN(N, Rows, Group, RowsLeft), 
        part(N, RowsLeft, RestGroups). 

/* Take first N elements of each of M lists into M lists. RestOf contains suffix of each of those lists (2D) */ 
take_firstN_fromEach(_, [], [], []) :- !.
take_firstN_fromEach(N, [H|T], [Result|RestOfResults], [RestOfH|RestOfT]) :- 
        take_firstN(N, H, Result, RestOfH), /* Take N from first list */
        take_firstN_fromEach(N, T, RestOfResults, RestOfT). /* Take N from other lists */


/* -------------------- TASK 2 --------------------- */

/*
  puzzle generated with grid size S x S.
  the puzzle contain from N to M known values.
  puzzle only has 1 unique solution. 
*/
find_puzzle_sudoku(Rows,S,N,M,B_R,B_C):- 
        N #>1, B_R >1, B_C>1, N #< M, S #= B_R * B_C, 

        length(BlankMtx, S),  
        maplist(same_length(BlankMtx), BlankMtx),

        % randomly generate N indices
        X = N,   
        U is (S*S)-1, 
        gen_list_unique_randoms(X, 0, U, FlatIndices), 
        maplist(dim1_to_dim2(S), FlatIndices, TwoDIndices), 
 
        % populate the N indices with random numbers, making sure no step invalidates the puzzle. 
        populate_matrix(TwoDIndices, BlankMtx, RandMtx, S, B_R, B_C), !, 

        % increase number of randomly filled slots from N to M 
        % as long as you can't generate a sudoku with unique solution.
        RandsLeft #= M - N, 
        LastDitchEffortCount is 20,     % if at M-1 slots filled, try to fill last slot LastDitchEffortCount times.
                                        % before giving up.  
        make_unique(S, B_R, B_C, RandsLeft, RandMtx, UnqMtx, LastDitchEffortCount), 
        Rows = UnqMtx.


gen_list_unique_randoms(0, _, _, []) :- !. 
gen_list_unique_randoms(N, L, U, [New|PrevRandoms]) :- 
        M is N-1,
        gen_list_unique_randoms(M, L, U, PrevRandoms), 
        gen_unique_random(L, U, PrevRandoms, New).  

% Given previously generated random numbers, generate a new random number that is not 
% already present in the previous list. 
gen_unique_random(L, U, PrevRandoms, X) :- 
        random_between(L, U, X), 
        all_distinct([X|PrevRandoms]), !. 
gen_unique_random(L, U, PrevRandoms, X) :- 
        gen_unique_random(L, U, PrevRandoms, X).

% helper function to go from 1D to 2D. 
dim1_to_dim2(NumCols, FlatIndex, [X, Y]) :- 
        X #= FlatIndex div NumCols, Y #= FlatIndex mod NumCols. 

% Clever code adapted from https://stackoverflow.com/a/35293467

% populate_matrix with randomly generated values, 
% given the indices to fill
populate_matrix([], Src, Src, _, _, _). 
populate_matrix([[R, C]|OtherIndices], PrevMtx, ResultMtx, S, B_R, B_C) :- 
        random_between(1, S, N), 
        change_row_X(R, C, N, PrevMtx, NewMtx),
        duplicate_term(NewMtx, CopyMtx), 
        gen_sudoku(CopyMtx, S, B_R, B_C), !, 
        populate_matrix(OtherIndices, NewMtx, ResultMtx, S, B_R, B_C). 

populate_matrix([[R, C]|OtherIndices], PrevMtx, ResultMtx, S, B_R, B_C) :-
        populate_matrix([[R, C]|OtherIndices], PrevMtx, ResultMtx, S, B_R, B_C).

change_row_X(R, C, N, PrevMtx, NewMtx) :- 
        same_length(PrevMtx, NewMtx), 
        append(Prefix, [OldRow|Suffix], PrevMtx),
        length(Prefix, R),
        change_item_X(OldRow, C, N, NewRow),  
        append(Prefix, [NewRow|Suffix], NewMtx). 

change_item_X(Elems, Idx, New, NewElems) :- 
        same_length(Elems, NewElems), 
        append(Prefix, [_|Suffix], Elems), 
        length(Prefix, Idx), 
        append(Prefix, [New|Suffix], NewElems).


count_atoms_in_mtx(Mtx, Result) :- maplist(count_atoms_in_list, Mtx, NumList), sum_list(NumList, Result). 

count_atoms_in_list([], 0) :- !. 
count_atoms_in_list([X|XS], Result) :- atomic(X), !, count_atoms_in_list(XS, Rest), Result is 1+Rest. 
count_atoms_in_list([_|XS], Result) :- count_atoms_in_list(XS, Result). 

% given a list, returns the 0-based index of the first non-atomic value. 
% TODO: can make more random by returning 1 of any of the non-atomic indices.  
first_non_atomic_index([], _) :- false. 
first_non_atomic_index([X|_], 0) :- not(atomic(X)), !.
first_non_atomic_index([_|XS], Result) :- first_non_atomic_index(XS, TempResult), Result is TempResult+1. 

% Takes a matrix and progressively fills it up until (RandsLeft=0 && TriesLeft=0) OR unique sudoku found. 
make_unique(S, B_R, B_C, _, SrcMtx, UnqMtx, _) :-
        duplicate_term(SrcMtx, TestMtx), 
        gen_sudoku(TestMtx, S, B_R, B_C), 
        count_atoms_in_mtx(TestMtx, NumAtoms), 
        MaxTerms is S*S, 
        NumAtoms is MaxTerms, !, % if
        UnqMtx = SrcMtx. 
make_unique(S, B_R, B_C, RandsLeft, SrcMtx, UnqMtx, TriesLeft) :- 
        RandsLeft > 0, !, 
        duplicate_term(SrcMtx, TestMtx), 
        gen_sudoku(TestMtx, S, B_R, B_C), %duplication of effort
        append(TestMtx, FlatList),
        first_non_atomic_index(FlatList, FlatIndex), % get me first non-atomic index, TODO increase randomness
        dim1_to_dim2(S, FlatIndex, Index2D), 
        populate_matrix([Index2D], SrcMtx, RandMtx, S, B_R, B_C),
        NewRandsLeft is RandsLeft-1,  
        make_unique(S, B_R, B_C, NewRandsLeft, RandMtx, UnqMtx, TriesLeft). 
make_unique(S, B_R, B_C, 0, SrcMtx, UnqMtx, TriesLeft) :- % last ditchEffort if M is reached and none found. 
        TriesLeft > 0, 
        NewTriesLeft is TriesLeft-1, 
        make_unique(S, B_R, B_C, 0, SrcMtx, UnqMtx, NewTriesLeft). 

