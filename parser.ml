open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
    match lookahead toks with
    | Some Tok_Let -> parse_let_expr toks 
    | Some Tok_If -> parse_if_expr toks
    | Some Tok_Fun -> parse_function_expr toks
    | Some Tok_Not -> parse_or_expr toks
    | Some Tok_Int _ -> parse_or_expr toks
    | Some Tok_Bool _ -> parse_or_expr toks
    | Some Tok_String _ -> parse_or_expr toks
    | Some Tok_ID _ -> parse_or_expr toks
    | Some Tok_LParen -> parse_or_expr toks
    | _ -> raise (InvalidInputException "parse_expr error")

and parse_let_expr toks =
    let toks = match_token toks Tok_Let in
    let (toks, recursion) = parse_recursion toks in
    let id = match lookahead toks with
    | Some Tok_ID x -> x 
    | _ -> raise (InvalidInputException "parse_let_expr error: bad tokid") in
    let toks = match_token toks (Tok_ID id) in
    let toks = match_token toks Tok_Equal in
    let (toks, e1) = parse_expr toks in
    let toks = match_token toks Tok_In in
    let (toks, e2) = parse_expr toks in
    (toks, Let (id, recursion, e1, e2))

and parse_recursion toks =
    match lookahead toks with
    | Some Tok_Rec -> (match_token toks Tok_Rec, true)
    | Some Tok_ID x -> (toks, false)
    | _ -> raise (InvalidInputException "parse let bad rec")

and parse_function_expr toks =
    let toks = match_token toks Tok_Fun in
    let id = match lookahead toks with
    | Some Tok_ID x -> x
    | _ -> raise (InvalidInputException "parse_function_expr: bad tokid") in
    let toks = match_token toks (Tok_ID id) in
    let toks = match_token toks Tok_Arrow in
    let (toks, e1) = parse_expr toks in
    (toks, Fun (id, e1))

and parse_if_expr toks =
    let toks = match_token toks Tok_If in
    let (toks, e1) = parse_expr toks in
    let toks = match_token toks Tok_Then in
    let (toks, e2) = parse_expr toks in
    let toks = match_token toks Tok_Else in
    let (toks, e3) = parse_expr toks in
    (toks, If (e1, e2, e3))

and parse_or_expr toks = 
    let (toks, e1) = parse_and_expr toks in
    match lookahead toks with
    | Some Tok_Or -> 
            let toks = match_token toks Tok_Or in
            let (toks, e2) = parse_or_expr toks in
            (toks, Binop (Or, e1, e2))
    | _ -> (toks, e1)

and parse_and_expr toks = 
    let (toks, e1) = parse_equality_expr toks in
    match lookahead toks with
    | Some Tok_And ->
            let toks = match_token toks Tok_And in
            let (toks, e2) = parse_and_expr toks in
            (toks, Binop (And, e1, e2))
    | _ -> (toks, e1)

and parse_equality_expr toks = 
    let (toks, e1) = parse_relational_expr toks in
    match lookahead toks with
    | Some Tok_Equal
    | Some Tok_NotEqual -> 
            let (toks, operation) = parse_equality_operator toks in
            let (toks, e2) = parse_equality_expr toks in
            (toks, Binop (operation, e1, e2))
    | _ -> (toks, e1)

and parse_equality_operator toks =
    match lookahead toks with
    | Some Tok_Equal -> (match_token toks Tok_Equal, Equal)
    | Some Tok_NotEqual -> (match_token toks Tok_NotEqual, NotEqual)
    | _ -> raise (InvalidInputException "bad equality")

and parse_relational_expr toks = 
    let (toks, e1) = parse_additive_expr toks in
    match lookahead toks with
    | Some Tok_Less
    | Some Tok_Greater
    | Some Tok_LessEqual
    | Some Tok_GreaterEqual -> 
            let (toks, operation) = parse_relational_operator toks in
            let (toks, e2) = parse_additive_expr toks in
            (toks, Binop (operation, e1, e2))
    | _ -> (toks, e1)
    
and parse_relational_operator toks = 
    match lookahead toks with
    | Some Tok_Less -> (match_token toks Tok_Less, Less)
    | Some Tok_Greater -> (match_token toks Tok_Greater, Greater)
    | Some Tok_LessEqual -> (match_token toks Tok_LessEqual, LessEqual)
    | Some Tok_GreaterEqual -> (match_token toks Tok_GreaterEqual, GreaterEqual)
    | _ -> raise (InvalidInputException "bad relational")

and parse_additive_expr toks = 
    let (toks, e1) = parse_multiplicative_expr toks in
    match lookahead toks with
    | Some Tok_Add
    | Some Tok_Sub ->
            let (toks, operation) = parse_additive_operator toks in
            let (toks, e2) = parse_additive_expr toks in
            (toks, Binop (operation, e1, e2))
    | _ -> (toks, e1)

and parse_additive_operator toks =
    match lookahead toks with
    | Some Tok_Add -> (match_token toks Tok_Add, Add)
    | Some Tok_Sub -> (match_token toks Tok_Sub, Sub)
    | _ -> raise (InvalidInputException "bad additive")

and parse_multiplicative_expr toks = 
    let (toks, e1) = parse_concat_expr toks in
    match lookahead toks with
    | Some Tok_Mult
    | Some Tok_Div ->
            let (toks, operation) = parse_multiplicative_operator toks in
            let (toks, e2) = parse_multiplicative_expr toks in
            (toks, Binop (operation, e1, e2))
    | _ -> (toks, e1)

and parse_multiplicative_operator toks =
    match lookahead toks with
    | Some Tok_Mult -> (match_token toks Tok_Mult, Mult)
    | Some Tok_Div -> (match_token toks Tok_Div, Div)
    | _ -> raise (InvalidInputException "bad multiplicative")

and parse_concat_expr toks = 
    let (toks, e1) = parse_unary_expr toks in
    match lookahead toks with
    | Some Tok_Concat ->
            let toks = match_token toks Tok_Concat in
            let (toks, e2) = parse_concat_expr toks in
            (toks, Binop (Concat, e1, e2))
    | _ -> (toks, e1)

and parse_unary_expr toks = 
    match lookahead toks with
    | Some Tok_Not ->
            let toks = match_token toks Tok_Not in
            let (toks, e1) = parse_unary_expr toks in
            (toks, Not (e1))
    | _ -> 
            parse_function_call_expr toks

and parse_function_call_expr toks =
    let (toks, e1) = parse_primary_expr toks in
    match lookahead toks with
    | Some Tok_Int _
    | Some Tok_Bool _
    | Some Tok_String _
    | Some Tok_ID _
    | Some Tok_LParen ->
            let (toks, e2) = parse_primary_expr toks in
            (toks, FunctionCall (e1, e2))
    | _ -> (toks, e1)

and parse_primary_expr toks = 
    match lookahead toks with
    | Some Tok_Int x -> 
            let toks = match_token toks (Tok_Int x) in
            (toks, Value (Int x))
    | Some Tok_Bool x -> 
            let toks = match_token toks (Tok_Bool x) in
            (toks, Value (Bool x))
    | Some Tok_String x -> 
            let toks = match_token toks (Tok_String x) in 
            (toks, Value (String x))
    | Some Tok_ID x -> 
            let toks = match_token toks (Tok_ID x) in 
            (toks, ID x)
    | Some Tok_LParen -> 
            let toks = match_token toks Tok_LParen in 
            let (toks, e1) = parse_expr toks in
            let toks = match_token toks Tok_RParen in
            (toks, e1)
    | _ -> raise (InvalidInputException "parse_primary_expr_error")

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
    match lookahead toks with
    | Some Tok_Def -> parse_def_mutop toks
    | Some Tok_DoubleSemi -> (match_token toks Tok_DoubleSemi, NoOp)
    | _ -> parse_expr_mutop toks
and parse_def_mutop toks =
    let toks = match_token toks Tok_Def in
    let id = match lookahead toks with
    | Some Tok_ID x -> x
    | _ -> raise (InvalidInputException "parse_def_mutop id error") in
    let toks = match_token toks (Tok_ID id) in
    let toks = match_token toks Tok_Equal in
    let (toks, e1) = parse_expr toks in
    let toks = match_token toks Tok_DoubleSemi in
    (toks, Def (id, e1))
and parse_expr_mutop toks =
    let (toks, e1) = parse_expr toks in
    let toks = match_token toks Tok_DoubleSemi in
    (toks, Expr (e1));;
