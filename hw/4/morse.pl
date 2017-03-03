% begin representation rules %

% separator:       0 or 00
% letter boundary: 000 or 00 or 0000
% word boundary:   0000000 or 00000+
% dih:             1 or 11
% dah:             111 or 11+

% canonical representations
representation([0,1], .). % dih
representation([0,0,0], ^). % letter boundary
representation([0,1,1|Rest], -) :- maplist(is(1), Rest). % dah
representation([0,0,0,0,0|Rest], #) :- maplist(is(0), Rest). % word boundary

% "corrupted" representations
representation([0,0], ^). % letter boundary
representation([0,1,1], .). % dih
representation([0,0,1], .). % dih
representation([0,0,1,1], .). % dih
representation([0,0,0,0], ^). % letter boundary
representation([0,0,1,1|Rest], -) :- maplist(is(1), Rest). % dah

% end representation rules %

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

% instantiate Rest to the tail of the given list
rest([_|Rest], Rest).

% split a List into all possible Prefixes and Suffixes
split(List, Prefix, Suffix) :-
  append(Prefix, Suffix, List).

% split a list into two parts where Prefix is composed of the items up to but 
% *not* including the item at index N, and Suffix is composed of the items 
% following index N.
% N.B. the item at index N is not a member of either of the two instantiated 
% lists
split_n(List, N, Prefix, RestSuffix) :-
  append(Prefix, Suffix, List),
  % length/2 is inclusive of the element at position N. We don't want This
  % element, so decrement N by one...
  NDec is N-1, 
  length(Prefix, NDec),
  % ...and chop one element off the Suffix list
  rest(Suffix, RestSuffix).

% assert that the given list contains no space characters ([^, #])
no_spaces([]).
no_spaces([First|Rest]) :- 
  Spaces = [^, #],
  \+ (member(First, Spaces)),
  no_spaces(Rest).

% assert that the given list contains no adjacent space characters ([^, #])
no_adjacent_spaces([]).
% one-item lists have no adjacent spaces
no_adjacent_spaces([_]).
% assert that the first two elements are not both spaces (& recurse)
no_adjacent_spaces([First,Second|Rest]) :-
  Spaces = [^, #],
  \+ (member(First, Spaces), member(Second, Spaces)),
  no_adjacent_spaces([Second|Rest]).

% instantiates a list without its last element
no_last([_], []).
no_last([H|T1], [H|T2]) :-
  no_last(T1, T2).

% given input I, instantiates Match and Suffix to all possible representations  
% of prefix(I) and their remaining Suffixes
match_morse_prefix(I, Match, Suffix) :- 
  split(I, Prefix, Suffix),
  representation(Prefix, Match).

% instantiates Match and Suffix to the character represented by the first 
% sublist of morse code before a space, with Suffix instantiated to the
% remaining unmatched input
match_message_prefix(I, [Match], Suffix) :- 
  nth(N, I, ^),
  split_n(I, N, Prefix, Suffix),
  morse(Match, Prefix).
% if this matching sublist ends in a hash, it represents a word boundary which
% should be preserved in our Match; construct a 2-element list composed of 
% Match and a hash
match_message_prefix(I, MatchHash, Suffix) :- 
  nth(N, I, #),
  split_n(I, N, Prefix, Suffix),
  morse(Match, Prefix),
  append([Match], [#], MatchHash).
% if the given input has no spaces, just try to instantiate the entire given
% input to a character and return an empty Suffix list
match_message_prefix(I, [Match], Suffix) :-
  no_spaces(I),
  morse(Match, I),
  length(Suffix, 0).

% greedily build the smallest available match, backtracing as necessary to
% ensure that all parts of the pattern are matched with a representation
% uses the accumulator technique
% http://www.cs.bham.ac.uk/~pjh/prolog_module/md5/md5_accumulators.html
signal_morse_helper([], O, O).
signal_morse_helper(I, Acc, O) :-
  match_morse_prefix(I, Match, Suffix),
  append(Acc, [Match], NewAcc),
  no_adjacent_spaces(NewAcc),
  signal_morse_helper(Suffix, NewAcc, O).

% if the First element of the given representation is zero, just call the
% helper with the input as-is...
leading_zero([First|Rest], [First|Rest]) :-
  First = 0.
% ...otherwise, because our representation for dihs and dahs requires that 
% they begin with a zero, append a zero to the beginning of the input 
% (implicit silence before recording begins)
leading_zero([First|Rest], O) :-
  First = 1,
  append([0], [First|Rest], O).

no_trailing_zero(I, I) :-
  last(I, Last),
  Last = 1.
no_trailing_zero(I, O) :-
  last(I, Last),
  Last = 0,
  no_last(I, O).

% signal_morse([1,1,1,0,1,1,1,0,0,0,1,1,1,0,1,1,1,0,1,1,1,0,0,0,1,0,1,1,1,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,0,0,1,1,1,0,1,1,1,0,1,1,1],M).
signal_morse(I, O) :-
  leading_zero(I, IL),
  no_trailing_zero(IL, IT),
  % dedupe results using method fix #2:
  % https://www.csee.umbc.edu/~finin/prolog/sibling/siblings.html
  setof(Opts, signal_morse_helper(IT, [], Opts), L), member(O, L).

% if the last token is a word boundary, stop
chew_error(Prefix, Prefix) :-
  last(Prefix, Last),
  member(Last, [#, error]).
% otherwise, recursively remove the last element of the list
chew_error(Prefix, PrefixNoError) :-
  no_last(Prefix, PrefixNoLast),
  chew_error(PrefixNoLast, PrefixNoError).

% if the last token is an error token, do not modify
swallow_error(Prefix, Prefix) :-
  last(Prefix, Last),
  Last \= error.
% if the last token is an error token and the second-to-last token is either a
% word boundary or error, stop
swallow_error(Prefix, Prefix) :-
  reverse(Prefix, [H1,H2|_]),
  H1 = error,
  member(H2, [#, error]).
% otherwise, if the last token is an error, remove it and start chewing
swallow_error(Prefix, PrefixNoError) :-
  last(Prefix, Last),
  Last = error,
  no_last(Prefix, PrefixNoLast),
  chew_error(PrefixNoLast, PrefixNoError).

% recursively Match input with a character up until the next space until the
% whole input has been represented or failure
signal_message_helper([], O, O).
signal_message_helper(I, Acc, O) :-
  match_message_prefix(I, Match, Suffix),
  append(Acc, Match, NewAcc),
  once(swallow_error(NewAcc, NewAccNoError)),
  signal_message_helper(Suffix, NewAccNoError, O).

% signal_message([1,1,1,0,1,1,1,0,0,0,1,1,1,0,1,1,1,0,1,1,1,0,0,0,1,0,1,1,1,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,0,0,1,1,1,0,1,1,1,0,1,1,1],M).
% signal_message([1,1,1,0,1,1,1,0,0,0,1,1,1,0,1,1,1,0,1,1,1,0,0,0,1,0,1,1,1,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,0,0,1,1,1,0,1,1,1,0,1,1,1,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,0,1,0,1,1,1,0,1,0,1,0,1,0],M).
signal_message(I, O) :-
  signal_morse(I, Morse),
  signal_message_helper(Morse, [], O).