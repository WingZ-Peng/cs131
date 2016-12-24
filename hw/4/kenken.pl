% allow discontigous grouping of constraint predicates for easier readability
% ref: http://stackoverflow.com/questions/7400904/discontiguous-predicate-warning-from-gnu-prolog#comment52673579_7403540
:-discontiguous(constrain_to_fence/2).
:-discontiguous(plain_constrain_to_fence/2).

% begin transpose %
%% sources: 
%% https://piazza.com/class/ij4pi2k5m0d5gn?cid=208 
%% http://stackoverflow.com/a/4281159
transpose([], []).
transpose([F|Fs], Ts) :-
transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
lists_firsts_rests(Ms, Ts, Ms1),
transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
lists_firsts_rests(Rest, Fs, Oss).
% end transpose %

constrain_domain(N, L) :-
  fd_domain(L, 1, N).

list_of_length(N, T) :-
  length(T, N).

distinct_one_to_n(N, T) :-
  % docs: http://www.gprolog.org/manual/html_node/gprolog062.html#sec325
  fd_all_different(T),
  % docs: http://www.gprolog.org/manual/html_node/gprolog060.html#Partial-AC%3A-%28%3A%3D%29%2F2
  maplist(constrain_domain(N), T).

% get the element El at given X and Y coordinates from 2D list T
% docs: http://www.gprolog.org/manual/html_node/gprolog044.html#sec220
get_element(T, Coords, El) :-
  split_coords(Coords, [X, Y]),
  nth(Y, T, Row),
  nth(X, Row, El).

% ref: http://stackoverflow.com/a/15577511
split_coords(X-Y, [X, Y]).

% begin arithmetic functions %

%% addition
%% the integer S is the sum of integers in the list L of squares.
constrain_to_fence(T, +(S,L)) :-
  sum(T, L, S).
% base fact: sum of the elements in an empty list is 0
sum(_, [], 0).
sum(T, [First|Rest], Sum) :-
  % instantiate El as the value of the coordinates in First
  get_element(T, First, El),  
  % instantiate NewSum recursively
  sum(T, Rest, NewSum),
  % subtract El's value from the given sum (will "count down" to 0)
  NewSum #= Sum - El.

%% multiplication
%% the integer P is the product of the integers in the list L of squares.
constrain_to_fence(T, *(P,L)) :-
  product(T, L, P).
% base fact: product of the elements in an empty list is 1 (identity operand)
product(_, [], 1).
product(T, [First|Rest], Product) :-
  % instantiate El as the value of the coordinates in First
  get_element(T, First, El),
  % instantiate NewProduct recursively
  product(T, Rest, NewProduct),
  % divide out El's value from the given sum (will reduce to 1)
  NewProduct #= Product / El.

%% subtraction
%% the integer D is the difference between the integer j in square J and the 
%% integer k in square K; D could be equal to either j−k or to k−j.
constrain_to_fence(T, -(D,J,K)) :-
  difference(T, J, K, D).
difference(T, Coord1, Coord2, Difference) :-
  get_element(T, Coord1, El1),
  get_element(T, Coord2, El2),
  (Difference #= El1 - El2; Difference #= El2 - El1).

%% division
%% the integer Q is the quotient of the integer j in square J and the integer
%% k in square K; Q could be equal to either j÷k or to k÷j. The remainder 
%% must be zero.
constrain_to_fence(T, /(Q,J,K)) :-
  quotient(T, J, K, Q).
quotient(T, Coord1, Coord2, Quotient) :-
  get_element(T, Coord1, El1),
  get_element(T, Coord2, El2),
  (Quotient #= El1 / El2; Quotient #= El2 / El1).

% end arithmetic functions %

% kenken/3
% N = number of rows and columns
% C = constraints (see spec)
% T = solution 2D list
kenken(N, C, T) :-
  % define "output" list T to be a 2D NxN list
  %% define for columns
  list_of_length(N, T),
  %% define for rows
  maplist(list_of_length(N), T),
  % define rows and columns to contain distinct integers in the range 0..N
  %% swap rows and columns
  transpose(T, T_transp),
  %% define for rows
  maplist(distinct_one_to_n(N), T),
  %% define for columns
  maplist(distinct_one_to_n(N), T_transp),
  % all constraints hold
  maplist(constrain_to_fence(T_transp), C),
  % assign concrete values (lowest possible) 
  % ref: https://piazza.com/class/ij4pi2k5m0d5gn?cid=180
  maplist(fd_labeling, T_transp).

% begin plain_kenken functions %

% create a list with numbers in the range 1..N
% ref: http://stackoverflow.com/a/1697019
% docs: http://www.gprolog.org/manual/gprolog.html#sec196
% docs: http://www.gprolog.org/manual/gprolog.html#sec113
create_range(Max, Range) :-
  findall(Val, between(1, Max, Val), Range).

% ref: https://piazza.com/class/ij4pi2k5m0d5gn?cid=198
plain_constrain_domain(Max, Val) :-
  create_range(Max, Range),
  member(Val, Range).

no_duplicates(L) :-
  % sort removes duplicate elements, so we can compare pre- and post-sorted
  % lengths to determine if T's elements are distinct
  % docs: http://www.gprolog.org/manual/gprolog.html#sort%2F2
  length(L, L_length),
  sort(L, L_sorted),
  length(L_sorted, L_sorted_length),
  (L_length == L_sorted_length).

plain_distinct_one_to_n(N, T) :-
  % N.B. constrain_domain must be ordered before no_duplicates so concrete
  % numbers are instantiated when no_duplicates does its check
  maplist(plain_constrain_domain(N), T),
  no_duplicates(T).

% begin plain arithmetic functions %

%% addition
%% the integer S is the sum of integers in the list L of squares.
plain_constrain_to_fence(T, +(S,L)) :-
  plain_sum(T, L, S).
% base fact: sum of the elements in an empty list is 0
plain_sum(_, [], 0).
plain_sum(T, [First|Rest], Sum) :-
  % instantiate El as the value of the coordinates in First
  get_element(T, First, El), 
  % subtract El's value from the given sum (will "count down" to 0)
  NewSum is Sum - El,
  % instantiate NewSum recursively
  plain_sum(T, Rest, NewSum).

%% multiplication
%% the integer P is the product of the integers in the list L of squares.
plain_constrain_to_fence(T, *(P,L)) :-
  plain_product(T, L, P).
% base fact: product of the elements in an empty list is 1.0 (identity operand)
% N.B. in this plain version the identity operand is represented as floating
% point because the result of division is a float, and apparently in GNU 
% Prolog, 1.0 != 1. Cf. constraint solver version in which the solver
% abstracts away this type comparison problem.
plain_product(_, [], 1.0).
plain_product(T, [First|Rest], Product) :-
  % instantiate El as the value of the coordinates in First
  get_element(T, First, El),
  % divide out El's value from the given sum (will reduce to 1)
  NewProduct is (Product / El),
  % instantiate NewProduct recursively
  plain_product(T, Rest, NewProduct).

%% subtraction
%% the integer D is the difference between the integer j in square J and the 
%% integer k in square K; D could be equal to either j−k or to k−j.
plain_constrain_to_fence(T, -(D,J,K)) :-
  plain_difference(T, J, K, D).
plain_difference(T, Coord1, Coord2, Difference) :-
  get_element(T, Coord1, El1),
  get_element(T, Coord2, El2),
  (Difference is El1 - El2; Difference is El2 - El1).

%% division
%% the integer Q is the quotient of the integer j in square J and the integer
%% k in square K; Q could be equal to either j÷k or to k÷j. The remainder 
%% must be zero.
plain_constrain_to_fence(T, /(Q,J,K)) :-
  plain_quotient(T, J, K, Q).
plain_quotient(T, Coord1, Coord2, Quotient) :-
  get_element(T, Coord1, El1),
  get_element(T, Coord2, El2),
  % compare using multiplication instead of division to avoid comparision 
  % incompatibility between integer and floating point
  (El1 is El2 * Quotient; El2 is El1 * Quotient).

% end plain arithmetic functions %

% plain_kenken/3
% N = number of rows and columns
% C = constraints (see spec)
% T = solution 2D list
plain_kenken(N, C, T) :-
  % define "output" list T to be a 2D NxN list
  %% define for columns
  list_of_length(N, T),
  %% define for rows
  maplist(list_of_length(N), T),
  % define rows and columns to contain distinct integers in the range 0..N
  %% swap rows and columns
  transpose(T, T_transp),
  %% define for rows
  maplist(plain_distinct_one_to_n(N), T),
  %% define for columns
  maplist(plain_distinct_one_to_n(N), T_transp),
  % all constraints hold
  maplist(plain_constrain_to_fence(T_transp), C).

% begin tests %

kenken_test_1(1,[]).

kenken_test_2(2,[]).

kenken_test_3(
  6,
  [
   +(11, [1-1, 2-1]),
   /(2, 1-2, 1-3),
   *(20, [1-4, 2-4]),
   *(6, [1-5, 1-6, 2-6, 3-6]),
   -(3, 2-2, 2-3),
   /(3, 2-5, 3-5),
   *(240, [3-1, 3-2, 4-1, 4-2]),
   *(6, [3-3, 3-4]),
   *(6, [4-3, 5-3]),
   +(7, [4-4, 5-4, 5-5]),
   *(30, [4-5, 4-6]),
   *(6, [5-1, 5-2]),
   +(9, [5-6, 6-6]),
   +(8, [6-1, 6-2, 6-3]),
   /(2, 6-4, 6-5)
  ]
).

kenken_test_4(
  4,
  [
   +(6, [1-1, 1-2, 2-1]),
   *(96, [1-3, 1-4, 2-2, 2-3, 2-4]),
   -(1, 3-1, 3-2),
   -(1, 4-1, 4-2),
   +(8, [3-3, 4-3, 4-4]),
   *(2, [3-4])
  ]
).

kenken_test_5(
  2,
  [
    *(4, [1-1, 1-2, 2-1])
  ]
).

/* end tests */

/*
Workspace/Notes

N = 2

2 1
1 2

1 2
2 1

N = 3

T = [[1,2,3],[2,3,1],[3,1,2]]
T = [[1,2,3],[3,1,2],[2,3,1]]
T = [[1,3,2],[2,1,3],[3,2,1]]
T = [[1,3,2],[3,2,1],[2,1,3]]
T = [[2,1,3],[1,3,2],[3,2,1]]
T = [[2,1,3],[3,2,1],[1,3,2]]
T = [[2,3,1],[1,2,3],[3,1,2]]
T = [[2,3,1],[3,1,2],[1,2,3]]
T = [[3,1,2],[1,2,3],[2,3,1]]
T = [[3,1,2],[2,3,1],[1,2,3]]
T = [[3,2,1],[1,3,2],[2,1,3]]
T = [[3,2,1],[2,1,3],[1,3,2]]

C = +(4, [1-1, 1-2]);
C = *(3, [1-1,1-2]);
C = -(2, 1-1, 1-2);
T = [[1,2,3],[3,1,2],[2,3,1]]
T = [[1,3,2],[3,2,1],[2,1,3]]
T = [[3,1,2],[1,2,3],[2,3,1]]
T = [[3,2,1],[1,3,2],[2,1,3]]


spec output: 
[[5,6,3,4,1,2],
 [6,1,4,5,2,3],
 [4,5,2,3,6,1],
 [3,4,1,2,5,6],
 [2,3,6,1,4,5],
 [1,2,5,6,3,4]]

my output:
[[5,6,4,3,2,1],
 [6,1,5,4,3,2],
 [3,4,2,1,6,5],
 [4,5,3,2,1,6],
 [1,2,6,5,4,3],
 [2,3,1,6,5,4]]

*/