open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
    match e with
    | Value (_) -> eval_value env e 
    | ID (_) -> eval_id env e
    | Fun (_, _) -> eval_fun env e
    | Not (_) -> eval_not env e
    | Binop (_, _, _) -> eval_binop env e
    | If (_, _, _) -> eval_if env e
    | FunctionCall (_, _) -> eval_function_call env e
    | Let (_, _, _, _) -> eval_let env e

and eval_value env e =
    match e with
    | Value (Int v) -> Int v
    | Value (Bool v) -> Bool v
    | Value (String v) -> String v
    | Value (Closure (env, var, expr)) -> Closure (env, var, expr)
    | _ -> raise (TypeError "Bad Value")

and eval_id env e = 
    match e with
    | ID name -> lookup env name
    | _ -> raise (TypeError "Invalid ID passed in.")

and eval_fun env e = 
    match e with
    | Fun (parameter, body) -> Closure (env, parameter, body)
    | _ -> raise (TypeError "Bad Fun")

and eval_not env e = 
    match e with
    | Not e1 -> 
            let expr = eval_expr env e1 in
            (match expr with
            | Bool b -> Bool (not b)
            | _ -> raise (TypeError "Boolean expected"))
    | _ -> raise (TypeError "Boolean expected")
            
and eval_binop env e =
    match e with
    | Binop (opr, e1, e2) ->
            let expr1 = eval_expr env e1 in
            let expr2 = eval_expr env e2 in
            (match opr, expr1, expr2 with
            | Add, Int x, Int y -> Int (x + y)
            | Sub, Int x, Int y -> Int (x - y)
            | Mult, Int x, Int y -> Int (x * y)
            | Div, Int x, Int y -> if y = 0 then raise DivByZeroError else Int (x / y)
            | Greater, Int x, Int y -> Bool (x > y)
            | Less, Int x, Int y -> Bool (x < y)
            | GreaterEqual, Int x, Int y -> Bool (x >= y)
            | LessEqual, Int x, Int y -> Bool (x <= y)
            | Concat, String x, String y -> String (x ^ y)
            | Equal, Int x, Int y -> Bool (x = y)
            | Equal, String x, String y -> Bool (x = y)
            | Equal, Bool x, Bool y -> Bool (x = y)
            | NotEqual, Int x, Int y -> Bool (x <> y)
            | NotEqual, String x, String y -> Bool (x <> y)
            | NotEqual, Bool x, Bool y -> Bool (x <> y)
            | Or, Bool x, Bool y -> Bool (x || y)
            | And, Bool x, Bool y -> Bool (x && y)
            | _ -> raise (TypeError "Bad Binop"))
    | _ -> raise (TypeError "Binop expected")

and eval_if env e = 
    match e with 
    | If (e1, e2, e3) ->
            let guard = eval_expr env e1 in
            (match guard with
            | Bool true -> eval_expr env e2
            | Bool false -> eval_expr env e3
            | _ -> raise (TypeError "Bool expected for if"))
    | _ -> raise (TypeError "Bad if")
    
and eval_function_call env e = 
    match e with
    | FunctionCall (e1, e2) ->
            let clsr = (match e1 with
            | ID x -> lookup env x
            | _ -> eval_expr env e1) in
            let value = eval_expr env e2 in
            (match clsr with
            | Closure (a, x, e3) ->
                    let a = extend a x value in
                    eval_expr a e3
            | _ -> raise (TypeError "Closure expected"))
    | _ -> raise (TypeError "Bad function call")


and eval_let env e = 
    match e with
    | Let (var, recursive, init, body) ->
            (match recursive with
            | true ->
                    let env = extend_tmp env var in
                    let value = eval_expr env init in
                    (update env var value; 
                    eval_expr env body)
            | false ->
                    let value = eval_expr env init in
                    let env = extend env var value in
                    eval_expr env body)

    | _ -> raise (TypeError "Bad Let")


(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
    match m with
    | Def (var, expr) ->
            let env = extend_tmp env var in
            let value = eval_expr env expr in
            (update env var value;
            (env, Some value))
    | Expr (e) -> (env, Some (eval_expr env e))
    | NoOp -> (env, None);;

