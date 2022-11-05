open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
    match e with
    | Number n1 -> 
      Int_Val n1

    | True -> 
      Bool_Val true

    | False -> 
      Bool_Val false

    | Var v1 -> 
      (match List.assoc_opt v1 env with 
      | None -> raise UndefinedVar
      | Some v -> v )
    
    | Plus (e1, e2) -> 
      let r1 = eval_expr e1 env in
      let r2 = eval_expr e2 env in
      (match r1, r2 with 
      | Int_Val i, Int_Val j ->  Int_Val (i + j)
      | _, _ -> raise TypeError)

    | Minus (e1, e2) -> 
      let r1 = eval_expr e1 env in
      let r2 = eval_expr e2 env in
      (match r1, r2 with 
      | Int_Val i, Int_Val j ->  Int_Val (i - j)
      | _, _ -> raise TypeError)

    | Times (e1, e2) -> 
      let r1 = eval_expr e1 env in
      let r2 = eval_expr e2 env in
      (match r1, r2 with 
      | Int_Val i, Int_Val j ->  Int_Val (i * j)
      | _, _ -> raise TypeError)

    | Div (e1, e2) -> 
      let r1 = eval_expr e1 env in
      let r2 = eval_expr e2 env in
      (match r1, r2 with 
      | Int_Val i, Int_Val j ->  
        if j = 0 then raise DivByZeroError 
        else Int_Val (i / j)
      | _, _ -> raise TypeError)
      
    | Mod (e1, e2) -> 
      let r1 = eval_expr e1 env in
      let r2 = eval_expr e2 env in
      (match r1, r2 with 
      | Int_Val i, Int_Val j ->  Int_Val (i mod j)
      | _, _ -> raise TypeError)

(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
    []
