open Hw2;;

(* convert_grammar test using grammar from hw1 tests *)

type awksub_hw1_nonterminals =
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

let awksub_test = convert_grammar awksub_grammar

(*
`awksub_test` should return the following:
(can't compare functions in OCaml; c.f.
http://www2.lib.uchicago.edu/keith/ocaml-class/operators.html#compare,
https://piazza.com/class/ij4pi2k5m0d5gn?cid=64)

  (Expr, 
   function
   | Expr -> 
     [[T"("; N Expr; T")"]; [N Num]; [N Expr; N Binop; N Expr]; [N Lvalue];
      [N Incrop; N Lvalue];[N Lvalue; N Incrop];]
   | Lvalue ->
    [[T"$"; N Expr];]
   | Incrop -> 
     [[T"++"]; [T"--"];]
   | Binop -> 
     [[T"+"]; [T"-"];]
   | Num -> 
     [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"]; [T"5"]; [T"6"]; [T"7"]; [T"8"]; 
      [T"9"]])
*)

let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

(* An example grammar for a small subset of Awk, derived from but not
   identical to the grammar in
   <http://web.cs.ucla.edu/classes/winter06/cs132/hw/hw1.html>.
   Note that this grammar is not the same as Homework 1; it is
   instead the same as the grammar under "Theoretical background"
   above.  *)

type awksub_nonterminals = 
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let test0 =
  (parse_prefix awkish_grammar accept_all ["ouch"]) = None

let test1 =
  (parse_prefix awkish_grammar accept_all ["9"])
   = Some ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "9"])], [])

let test2 =
  (parse_prefix awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"])
   = Some
       ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "9"]);
	 (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Lvalue]);
	 (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);
	 (Num, [T "1"])],
	["+"])

let test3 =
  (parse_prefix awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
   = None

(* This one might take a bit longer.... *)
let test4 =
 (parse_prefix awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
  = Some
     ([(Expr, [N Term; N Binop; N Expr]); (Term, [T "("; N Expr; T ")"]);
       (Expr, [N Term]); (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
       (Expr, [N Term]); (Term, [N Num]); (Num, [T "8"]); (Binop, [T "-"]);
       (Expr, [N Term; N Binop; N Expr]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "--"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Num]); (Num, [T "9"]); (Binop, [T "+"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
       (Expr, [N Term; N Binop; N Expr]); (Term, [N Incrop; N Lvalue]);
       (Incrop, [T "++"]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "2"]); (Binop, [T "+"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Num]);
       (Num, [T "8"]); (Binop, [T "-"]); (Expr, [N Term]); (Term, [N Num]);
       (Num, [T "9"]); (Binop, [T "-"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Incrop; N Lvalue]);
       (Incrop, [T "++"]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "5"]); (Incrop, [T "++"]); (Incrop, [T "++"]);
       (Incrop, [T "--"]); (Binop, [T "-"]); (Expr, [N Term]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]);
       (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "8"]); (Incrop, [T "++"]); (Incrop, [T "++"]);
       (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "0"])],
      [])

let rec contains_lvalue = function
  | [] -> false
  | (Lvalue,_)::_ -> true
  | _::rules -> contains_lvalue rules

let accept_only_non_lvalues rules frag =
  if contains_lvalue rules
  then None
  else Some (rules, frag)

let test5 =
  (parse_prefix awkish_grammar accept_only_non_lvalues
      ["3"; "-"; "4"; "+"; "$"; "5"; "-"; "6"])
   = Some
      ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
	(Binop, [T "-"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
       ["+"; "$"; "5"; "-"; "6"])

(* my test cases *)

(* a test using a converted grammar from HW 1 *)
type archer_nonterminals =
  | Utterance | Complaint | Quip | Shout | Person

(* N.B. this grammar has blind alley rules *)
let archer_grammar = 
  (Utterance,
  function
    | Utterance -> [[N Complaint]; [N Quip]; [N Shout]; [N Person]]
    | Complaint -> [[T"Get it together, "; N Person]; 
                    [T"I'm kind of in the middle of something, "; N Person]]
    | Quip -> [[T"You're entering the Danger Zone!"]; 
              [T"Prasing?"]; [T"You're not my supervisor!"]; 
              [T"Seriously guys have we stopped doing 'phrasing'?"];]
    | Shout -> [[T"Lana!"; N Shout]; [T"Danger Zone!"; N Shout];] (* blind alley rules *)
    | Person -> [[T"Cyril"]; [T"Cheryl"]; [T"Krieger"]; [T"Mother"]])

(* test parse_prefix is not able to derive anything for a string that would
   require following blind alley rules (impossible to arrive at all 
   terminal rules) *)
let test_1 = (parse_prefix archer_grammar accept_all ["Lana!"; "Lana!"; "Lana!";]) = None

(* a test using an ambiguous grammar *)

type english_nonterminals =
  | Sentence | DeterminerPhrase | Determiner | Noun | VerbPhrase | Verb | PrepositionalPhrase | Preposition

let ambiguous_english_grammar = 
  (Sentence,
  function
    | Sentence -> [[N DeterminerPhrase; N VerbPhrase]; [N DeterminerPhrase; N VerbPhrase]]
    | DeterminerPhrase -> [[N Determiner; N Noun; N PrepositionalPhrase]; [N Determiner; N Noun]; [N Determiner]]
    | Determiner -> [[T"a"]; [T"the"]; [T"I"]]
    | Noun -> [[T"man"]; [T"woman"]; [T"telescope"]]
    | VerbPhrase -> [[N Verb; N DeterminerPhrase]; [N Verb; N DeterminerPhrase; N PrepositionalPhrase]]
    | Verb -> [[T"saw"]; [T"kissed"]; [T"punched"]]
    | PrepositionalPhrase -> [[N Preposition; N DeterminerPhrase]; [N Preposition]]
    | Preposition -> [[T"with"]; [T"using"]; [T"on"]])

(* 
The toy grammar above can be used to derive the sentence sought below, "I saw 
the man with the telescope," in two ways which correspond two possible 
interpretations of the ambiguous sentence. They mean, in paraphrase:
(1) By using a telescope, I was able to see the man. 
(2) I saw the man, who was holding a telescope.

Though the grammar is ambiguous, parse_prefix derives interpetation (2) every
time due to the rule ordering.
 *)

let test_2 = (parse_prefix ambiguous_english_grammar accept_all 
                      ["I"; "saw"; "the"; "man"; "with"; "the"; "telescope"])
           = Some
           ([(Sentence, [N DeterminerPhrase; N VerbPhrase;]); 
             (DeterminerPhrase, [N Determiner]); (Determiner, [T"I"]);
             (VerbPhrase, [N Verb; N DeterminerPhrase]);
             (Verb, [T"saw"]); (DeterminerPhrase, [N Determiner; N Noun; N PrepositionalPhrase]);
             (Determiner, [T"the"]); (Noun, [T"man"]); 
             (PrepositionalPhrase, [N Preposition; N DeterminerPhrase]);
             (Preposition, [T"with"]); (DeterminerPhrase, [N Determiner; N Noun]);
             (Determiner, [T"the"]); (Noun, [T"telescope"])], [])
