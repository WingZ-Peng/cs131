% separator:       0 or 00
% letter boundary: 000 or 00 or 0000
% word boundary:   0000000 or 00000+
% dih:             1 or 11
% dah:             111 or 11+

% unambiguous representations
% representation([], []).
% representation([0], s). % separates adjancent dih and dah
representation([0,1], .). % dih
representation([0,1,1|Rest], -) :- maplist(is(1), Rest). % dah
representation([0,0,0], ^). % letter boundary
representation([0,0,0,0,0|Rest], #) :- maplist(is(0), Rest). % word boundary

% ambiguous representations
representation([0,0], s). % separates adjancent dih and dah
representation([0,1,1], .). % dih
representation([0,0], ^). % letter boundary
representation([0,0,0,0], ^). % letter boundary

% begin morse knowledge base from spec %

morse(a, [.,-]).           % A
morse(b, [-,.,.,.]).	   % B
morse(c, [-,.,-,.]).	   % C
morse(d, [-,.,.]).	   % D
morse(e, [.]).		   % E
morse('e''', [.,.,-,.,.]). % É (accented E)
morse(f, [.,.,-,.]).	   % F
morse(g, [-,-,.]).	   % G
morse(h, [.,.,.,.]).	   % H
morse(i, [.,.]).	   % I
morse(j, [.,-,-,-]).	   % J
morse(k, [-,.,-]).	   % K or invitation to transmit
morse(l, [.,-,.,.]).	   % L
morse(m, [-,-]).	   % M
morse(n, [-,.]).	   % N
morse(o, [-,-,-]).	   % O
morse(p, [.,-,-,.]).	   % P
morse(q, [-,-,.,-]).	   % Q
morse(r, [.,-,.]).	   % R
morse(s, [.,.,.]).	   % S
morse(t, [-]).	 	   % T
morse(u, [.,.,-]).	   % U
morse(v, [.,.,.,-]).	   % V
morse(w, [.,-,-]).	   % W
morse(x, [-,.,.,-]).	   % X or multiplication sign
morse(y, [-,.,-,-]).	   % Y
morse(z, [-,-,.,.]).	   % Z
morse(0, [-,-,-,-,-]).	   % 0
morse(1, [.,-,-,-,-]).	   % 1
morse(2, [.,.,-,-,-]).	   % 2
morse(3, [.,.,.,-,-]).	   % 3
morse(4, [.,.,.,.,-]).	   % 4
morse(5, [.,.,.,.,.]).	   % 5
morse(6, [-,.,.,.,.]).	   % 6
morse(7, [-,-,.,.,.]).	   % 7
morse(8, [-,-,-,.,.]).	   % 8
morse(9, [-,-,-,-,.]).	   % 9
morse(., [.,-,.,-,.,-]).   % . (period)
morse(',', [-,-,.,.,-,-]). % , (comma)
morse(:, [-,-,-,.,.,.]).   % : (colon or division sign)
morse(?, [.,.,-,-,.,.]).   % ? (question mark)
morse('''',[.,-,-,-,-,.]). % ' (apostrophe)
morse(-, [-,.,.,.,.,-]).   % - (hyphen or dash or subtraction sign)
morse(/, [-,.,.,-,.]).     % / (fraction bar or division sign)
morse('(', [-,.,-,-,.]).   % ( (left-hand bracket or parenthesis)
morse(')', [-,.,-,-,.,-]). % ) (right-hand bracket or parenthesis)
morse('"', [.,-,.,.,-,.]). % " (inverted commas or quotation marks)
morse(=, [-,.,.,.,-]).     % = (double hyphen)
morse(+, [.,-,.,-,.]).     % + (cross or addition sign)
morse(@, [.,-,-,.,-,.]).   % @ (commercial at)

% Error.
morse(error, [.,.,.,.,.,.,.,.]). % error - see below

% Prosigns.
morse(as, [.,-,.,.,.]).          % AS (wait A Second)
morse(ct, [-,.,-,.,-]).          % CT (starting signal, Copy This)
morse(sk, [.,.,.,-,.,-]).        % SK (end of work, Silent Key)
morse(sn, [.,.,.,-,.]).          % SN (understood, Sho' 'Nuff)

% end morse knowledge base %

% partition_input_once(I, O1, O2) :- 
%   sublist(O1, I), 
%   sublist(O2, I), 
%   equal_length(O1, O2),
%   O1 \= O2.

% partition_input_once([], [], []).
% partition_input_once([IFirst|IRest], [IFirst|O1], O2) :- 
%   partition_input_once(IRest, O1, O2).
% partition_input_once([IFirst|IRest], O1, [IFirst|O2]) :- 
%   partition_input_once(IRest, O1, O2).

% partition_input_all([], []).
% partition_input_all([IFirst|IRest], [[IFirst|ORest]|Pss]) :-
%   partition_input_once(IRest, ORest, Res),
%   partition_input_all(Res, Pss).

% list_n_parts(List, Parts, Result) :-
%     length(Result, Parts),
%     append(Result, List).

% lists_identical([], []).
% lists_identical([First1|Rest1], [First2|Rest2]) :-
%   First1 = First2, lists_identical(Rest1, Rest2).

% append_all(Prefix, Lists, Prefixedlists) :-
%   maplist(append(Prefix), Lists, Prefixedlists).

% trace. list_partitioned([a,b,c], O).
% list_partitioned([], []).
% list_partitioned(I, O) :-
%  lists_identical(I, O).
% list_partitioned([First|RestI], [First|RestO]) :-
%   append_all([First], list_partitioned(RestI, RestO), First).

% div(L, A, B) :-
%     append(A, B, L),
%     length(A, N),
%     length(B, N).

% split([a,b,c,d], O).
% split([], []).
% split([First | Tail], [[First] | Rest]) :-
%     split(Tail, Rest).
% split([First | Tail], [First | Rest]) :-
%    split(Tail, Rest).

% part([a,b,c,d], 2, O).
% part([], 0, []).
% part(L, N, [DL|DLTail]) :-
%    length(DL, N),
%    append(DL, LTail, L),
%    part(LTail, (X is (N - 1)), DLTail).

% list has no items
empty([], true).
% list has at least one item
empty([_|_], false).

% split a List into all possible Prefixes and Suffixes
split(List, Prefix, Suffix) :-
  append(Prefix, Suffix, List).
%  empty(Prefix, false),
%   empty(Suffix, false).

% partitioned(List, Output) :-
%   split(List, Prefix, Suffix),
%   partitioned(Prefix, RecPrefix),
%   partitioned(Suffix, RecSuffix),
%   append(RecPrefix, RecSuffix, Output).
% partitioned(List, [List]).

% given input I, instantiates Match and Suffix to all possible representations  
% of prefix(I) and their remaining Suffixes
% match_prefix([], [], []).
match_prefix(I, Match, Suffix) :- 
  split(I, Prefix, Suffix),
  representation(Prefix, Match).

% signal_morse([0,1,1,1,0,1,1,1,0,0,0,1,1,1,0,1,1,1,0,1,1,1,0,0,0,1,0,1,1,1,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,0,0,1,1,1,0,1,1,1,0,1,1,1,0],M).
% signal_morse([], O).
% signal_morse(I, O) :-
%   match_prefix(I, Match, Suffix),
%   empty(Match, false),
%   empty(Suffix, true).
signal_morse(I, O) :-
  match_prefix(I, Match, Suffix),
  append([Match], O, NewO),
  signal_morse(Suffix, NewO).

%  maplist(representation, P, O).


signal_message(I, O).



app([], X, X).
app([H|T], Y, [H|Z]) :- app(T, Y, Z).
encode([], []).
encode([Letter|Text], Coding) :-
  translate(Letter, Code),
  app(Code, RestCode, Coding),
  encode(Text, RestCode).