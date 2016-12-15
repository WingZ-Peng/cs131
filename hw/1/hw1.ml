(* typings from spec *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec find a b = 
  match b with
  | [] -> false
  | first::rest -> 
    if a = first then true
    else find a rest
;;

let rec subset a b =
  match a with
  | [] -> true
  | first::rest ->
    if find first b then subset rest b
    else false
;;

let rec equal_sets a b = 
  subset a b && subset b a
;;

let rec set_union a b = 
  match a with
  | [] -> b
  | first::rest ->
    if find first b then set_union rest b
    else set_union rest (first::b)
;;

let rec set_intersection a b = 
  match a with
  | [] -> []
  | first::rest ->
    if find first b then first::(set_intersection rest b)
    else set_intersection rest b
;;

let rec set_diff a b =
  match a with
  | [] -> []
  | first::rest ->
    if find first b then set_diff rest b
    else first::(set_diff rest b)
;;

(* follow this method recursively: 
(1) https://mat.iitm.ac.in/home/sryedida/public_html/caimna/transcendental/iteration%20methods/fixed-point/iteration.html 
Per last year's Piazza, don't worry about infinite loop/stack overflow 
(2) https://piazza.com/class/ij4pi2k5m0d5gn?cid=27 i.e. don't set fixed limit
like link #1 recommends
*) 
let rec computed_fixed_point eq f x =
  (* compare f(x) `eq` x; if it does, we're done *)
  if (eq (f x) x) then x
  (* else call recursively with f(x) as the new value for x;
     cf. https://piazza.com/class/ij4pi2k5m0d5gn?cid=35 *)
  else computed_fixed_point eq f (f x) 
;;

(* using explicit typings to get nice tracebacks for debugging; cf.
   https://ocaml.org/learn/tutorials/debug.html#Polymorphicfunction *)
let rec computed_periodic_point eq f (p: int) x =
  (* period is zero: no iterations to perform so return x *)
  if (p = 0) then x 
  else
      (* search for a perodic point decrementing period by 1 and f(x) = x.
         If when we feed the reuslt to f, it is equal to x, then we're done *)
      if (eq (f(computed_periodic_point eq f (p-1) (f x))) x) then x
      (* otherwise recurse with x as f(x) *)
      else computed_periodic_point eq f p (f x)
;;


(* filter_blind_alleys helper functions *)

(* utility functions to get the lhs and rhs of tuples (i.e. grammar rules) 
   cf. http://stackoverflow.com/a/26005812
*)
let get_lhs (x, _) = x
;;

let get_rhs (_, x) = x
;;

(* map function that takes a list of 2-tuples and returns a list of the 
   contents of each tuple's first (index 0) element *)
let rec flatten_lhs rules =
  match rules with
  | [] -> []
  | first::rest ->
    (N(get_lhs first))::(flatten_lhs rest)
;;

(* true if given rule is terminal, false if nonterminal *)
let is_terminal_rule rule = 
  match rule with
  | T x -> true
  | N x -> false
;;

(* given a rule's rhs, does it contain at least one nonterminal rule? *)
let rec has_nonterminal rhs = 
  match rhs with
  | [] -> false
  | first::rest ->
    if is_terminal_rule first then
      has_nonterminal rest
    else true
;;

(* takes a rule rhs and removes terminal rules from it, leaving only 
   nonterminals *)
let rec remove_terminals rhs = 
  match rhs with 
  | [] -> []
  | first::rest ->
    if is_terminal_rule first then
      remove_terminals rest
    else
      first::(remove_terminals rest)
;;

(* given a grammar, returns all rules containing only terminals *)
let rec terminal_rules g = 
  match g with
  | [] -> g
  | (lhs, rhs)::rest ->
    if not (has_nonterminal rhs) then (lhs, rhs)::(terminal_rules rest)
    else terminal_rules rest
;;

(* Winter 2016 TA on Piazza said to do something like this, didn't end up
   using it ¯\_(ツ)_/¯
let rec good_rules unknown_rules terminal_rules = 
  computed_fixed_point equal_sets terminal_rules
;;
*)

(* uses the lhs of rules known to terminate to find rules in `unknown_rules`
   whose rhs is composed entirely of terminating rules and is thus a 
   terminating rule itself *)
let rec good_rules unknown_rules terminating_rules =
  match unknown_rules with
  | [] -> terminating_rules
  | (lhs, rhs)::rest ->
    (* if the rhs of this unknown rule is a subset of the rules known to 
       terminate, and it's not already in the list of terminating rules, add 
       it to terminating rules and recurse *)
    if subset (remove_terminals rhs) (flatten_lhs terminating_rules) && 
       (not (find (lhs, rhs) terminating_rules)) then
      good_rules rest ((lhs, rhs)::terminating_rules)
    else good_rules rest terminating_rules
;;

(* repeatedly calls good_rules with given undetermined rules and terminating 
   rules until good_rules yeilds no new terminating rules. it's basically 
   BFS from the ground (i.e. terminal rules) up *)
let rec build_good_rules unknown_rules terminating_rules = 
  let terminals = good_rules unknown_rules terminating_rules in
  if (List.length terminals) = (List.length terminating_rules) then
    terminals
  else
    build_good_rules unknown_rules terminals
;;

(* order the `good_rules` subset of `rules` in the order of the rules in 
   `rules`. Needed because our build_good_rules implementation doesn't 
   maintain the order of rules as required by the spec *)
let rec reorder_rules rules good_rules =
  match rules with
  | [] -> []
  | first::rest ->
    if find first good_rules then
      first::(reorder_rules rest good_rules)
    else
      reorder_rules rest good_rules
;;

(* where it all comes together: takes a grammar, splits it into its start 
   symbol and following rules, builds good rules from known terminals, 
   reconstructs the correct order, and merges them back together with the 
   start symbol *)
let filter_blind_alleys g = 
  let rules = get_rhs g in
  let terminals = terminal_rules rules in
  (* `unknown_rules` (first arg to `build_good_rules`) is rules minus known 
      terminals *)
  get_lhs g, (reorder_rules rules (build_good_rules 
    (set_diff rules terminals) terminals))
;;