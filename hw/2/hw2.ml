(* typings from HW 1 *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* utility functions to get the lhs and rhs of tuples (i.e. grammar rules) 
   cf. http://stackoverflow.com/a/26005812
*)
let get_lhs (x, _) = x
;;

let get_rhs (_, x) = x
;;

(* begin warmup functions *)

let rec convert_rules nonterminal rules =
  match rules with
  | [] -> []
  | (lhs, rhs)::rest ->
    if nonterminal = lhs then
      rhs::(convert_rules nonterminal rest)
    else
      convert_rules nonterminal rest
;;

let convert_grammar gram1 = 
  get_lhs gram1, (function nt -> convert_rules nt (get_rhs gram1))
;;

(* begin parse_prefix functions *)

(* true if given rule is terminal, false if nonterminal *)
let is_terminal_rule rule = 
  match rule with
  | T x -> true
  | N x -> false
;;

(* avoid mutual recursion by encapsulating our two recursive functions in a 
   `matcher` function 
   technique: https://piazza.com/class/ij4pi2k5m0d5gn?cid=91
   conceptual explanation: https://piazza.com/class/ij4pi2k5m0d5gn?cid=78 *)
let rec matcher rule_function start rule_list acceptor deriv fragment = 
  (* "In charge of calling the acceptor after it's finished matching all 
  possible prefixes. It also iterates through the list (h::t) and checks if 
  the symbol is a terminal or non-terminal symbol. If it's terminal, it's 
  simple. Match it against the fragment, if it matches then just call 
  check_symbol again with your new rhs (the remaining tail of the list) and 
  fragment." *)
  let rec check_symbols g rules acceptor deriv fragment = 
    match rules with 
    (* no more rules, call acceptor with deriv and fragment *)
    | [] -> acceptor deriv fragment
    | N first::rest ->
      (* call matcher, replacing acceptor with a recursive call to
          `check_symbols` with `deriv` and `fragment` omitted
          https://piazza.com/class/ij4pi2k5m0d5gn?cid=94 *)
      matcher g first (g first) (check_symbols g rest acceptor) deriv fragment
    | T first::rest ->
      match fragment with
      | [] -> None
      | first'::rest' ->
        (* compare first fragment symbol with first rule list symbol *)
        if (first' = first) then
          (* continue with the rest of the fragment *)
          check_symbols g rest acceptor deriv rest'
        else None
  in
  (* "It takes the first rhs (something like [T "3"; N Num]) and calls 
    check_symbols. Then it just checks if the return value is in the form of 
    Some(a) or None. If it's None, go to the next rhs." *)
  let rec check_rhs rule_function start rule_list acceptor deriv fragment =
    match rule_list with 
    | [] -> None
    | first::rest ->
      (* concatenate lists (i.e. append to list) with @ operator, cf. 
         https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VAL(@) *)
      let new_deriv = deriv@[(start, first)] in
      match check_symbols rule_function first acceptor new_deriv fragment with
      | None -> (* check failed, recurse with next rhs *)
        check_rhs rule_function start rest acceptor deriv fragment
      | Some(d, s) -> Some(d, s) (* successful check, we're done *)
  in
  (* call `check_rhs` to start things off *)
  check_rhs rule_function start rule_list acceptor deriv fragment
;;

let parse_prefix g acceptor fragment = 
  let start = (get_lhs g) in
  let rule_function = (get_rhs g) in
  matcher rule_function start (rule_function start) acceptor [] fragment
;;