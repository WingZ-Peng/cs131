open Hw1;;

let subset_test0 = assert(subset [] [1;2;3])
let subset_test1 = assert(subset [3;1;3] [1;2;3])
let subset_test2 = assert(not (subset [1;3;7] [4;1;3]))
let my_subset_test_0 = assert(subset [] [])
let my_subset_test_1 = assert(subset [3;3] [3])

let equal_sets_test0 = assert(equal_sets [1;3] [3;1;3])
let equal_sets_test1 = assert(not (equal_sets [1;3;4] [3;1;3]))
let my_equal_sets_test0 = assert(equal_sets [] [])
let my_equal_sets_test1 = assert(equal_sets [1;2;3] [3;2;1])

let set_union_test0 = assert(equal_sets (set_union [] [1;2;3]) [1;2;3])
let set_union_test1 = assert(equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3])
let set_union_test2 = assert(equal_sets (set_union [] []) [])
let my_set_union_test0 = assert(equal_sets (set_union [1] []) [1])
let my_set_union_test1 = assert(equal_sets (set_union [1;1] [1;1]) [1])

let set_intersection_test0 =
  assert(equal_sets (set_intersection [] [1;2;3]) [])
let set_intersection_test1 =
  assert(equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3])
let set_intersection_test2 =
  assert(equal_sets (set_intersection [1;2;3;4] [3;1;2;4]) [4;3;2;1])
let my_set_intersection_test0 =
  assert(equal_sets (set_intersection [1] [1]) [1])
let my_set_intersection_test1 =
  assert(equal_sets (set_intersection [5] [5;5;5]) [5])

let set_diff_test0 = assert(equal_sets (set_diff [1;3] [1;4;3;1]) [])
let set_diff_test1 = assert(equal_sets (set_diff [4;3;1;1;3] [1;3]) [4])
let set_diff_test2 = assert(equal_sets (set_diff [4;3;1] []) [1;3;4])
let set_diff_test3 = assert(equal_sets (set_diff [] [4;3;1]) [])
let my_set_diff_test0 = assert(equal_sets (set_diff [4] [4]) [])
let my_set_diff_test1 = assert(equal_sets (set_diff [4;4] [4]) [])

let computed_fixed_point_test0 =
  assert(computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0)
let computed_fixed_point_test1 =
  assert(computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity)
let computed_fixed_point_test2 =
  assert(computed_fixed_point (=) sqrt 10. = 1.)
let computed_fixed_point_test3 =
  assert(((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 (fun x -> x /. 2.)
			 10.)
   = 1.25))
let my_computed_fixed_point_test0 =
  assert(computed_fixed_point (=) (fun x -> x / 128) 256 = 0)

let computed_periodic_point_test0 =
  assert(computed_periodic_point (=) (fun x -> x / 2) 0 (-1) = -1)
let computed_periodic_point_test1 =
  assert(computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5 = -1. )
let my_computed_periodic_point_test0 =
  assert(computed_periodic_point (=) (fun x -> x / -2) 0 (1) = 1)

(* An example grammar for a small subset of Awk, derived from but not
   identical to the grammar in
   <http://web.cs.ucla.edu/classes/winter06/cs132/hw/hw1.html>.  *)


type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let awksub_test0 =
  assert(filter_blind_alleys awksub_grammar = awksub_grammar)

let awksub_test1 =
  filter_blind_alleys (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules)

let awksub_test2 =
  assert(filter_blind_alleys (Expr,
      [Expr, [N Num];
       Expr, [N Lvalue];
       Expr, [N Expr; N Lvalue];
       Expr, [N Lvalue; N Expr];
       Expr, [N Expr; N Binop; N Expr];
       Lvalue, [N Lvalue; N Expr];
       Lvalue, [N Expr; N Lvalue];
       Lvalue, [N Incrop; N Lvalue];
       Lvalue, [N Lvalue; N Incrop];
       Incrop, [T"++"]; Incrop, [T"--"];
       Binop, [T"+"]; Binop, [T"-"];
       Num, [T"0"]; Num, [T"1"]; Num, [T"2"]; Num, [T"3"]; Num, [T"4"];
       Num, [T"5"]; Num, [T"6"]; Num, [T"7"]; Num, [T"8"]; Num, [T"9"]])
  = (Expr,
     [Expr, [N Num];
      Expr, [N Expr; N Binop; N Expr];
      Incrop, [T"++"]; Incrop, [T"--"];
      Binop, [T "+"]; Binop, [T "-"];
      Num, [T "0"]; Num, [T "1"]; Num, [T "2"]; Num, [T "3"]; Num, [T "4"];
      Num, [T "5"]; Num, [T "6"]; Num, [T "7"]; Num, [T "8"]; Num, [T "9"]]))

let awksub_test3 =
  assert(filter_blind_alleys (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    filter_blind_alleys (Expr, List.tl (List.tl awksub_rules)))

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let giant_test0 =
  assert(filter_blind_alleys giant_grammar = giant_grammar)

let giant_test1 =
  assert(filter_blind_alleys (Sentence, List.tl (snd giant_grammar)) =
    (Sentence,
     [Quiet, []; Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]]))

let giant_test2 =
  assert(filter_blind_alleys (Sentence, List.tl (List.tl (snd giant_grammar))) =
    (Sentence,
     [Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Grunt]; Sentence, [N Shout]]))


(* My filter_blind_alleys test, cf. http://www.imdb.com/title/tt1486217/ *)

type archer_nonterminals =
  | Utterance | Complaint | Quip | Shout | Person

let archer_grammar = 
  Utterance,
  [Complaint, [T"Get it together, "; N Person];
   Complaint, [T"I'm kind of in the middle of something, "; N Person];
   Quip, [T"You're entering the Danger Zone!"];
   Quip, [T"Prasing?"];
   Quip, [T"You're not my supervisor!"];
   Quip, [T"Seriously guys have we stopped doing 'phrasing'?"];
   Shout, [T"Lana!"; N Shout]; (* blind alley rule *)
   Shout, [T"Danger Zone!"; N Shout]; (* blind alley rule *)
   Person, [T"Cyril"];
   Person, [T"Cheryl"];
   Person, [T"Krieger"];
   Person, [T"Mother"]]
  
let archer_test0 = assert(filter_blind_alleys archer_grammar = 
  (Utterance,
    [Complaint, [T"Get it together, "; N Person];
    Complaint, [T"I'm kind of in the middle of something, "; N Person];
    Quip, [T"You're entering the Danger Zone!"];
    Quip, [T"Prasing?"];
    Quip, [T"You're not my supervisor!"];
    Quip, [T"Seriously guys have we stopped doing 'phrasing'?"];
    Person, [T"Cyril"];
    Person, [T"Cheryl"];
    Person, [T"Krieger"];
    Person, [T"Mother"]]))