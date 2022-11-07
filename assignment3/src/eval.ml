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

    | And (e1, e2) -> 
      let r1 = eval_expr e1 env in
      let r2 = eval_expr e2 env in
      (match r1, r2 with 
      | Bool_Val i, Bool_Val j ->  Bool_Val (i && j)
      | _, _ -> raise TypeError)

    | Or (e1, e2) -> 
      let r1 = eval_expr e1 env in
      let r2 = eval_expr e2 env in
      (match r1, r2 with 
      | Bool_Val i, Bool_Val j ->  Bool_Val (i || j)
      | _, _ -> raise TypeError)
    
    | Not e1 -> 
      let r1 = eval_expr e1 env in
      (match r1 with 
      | Bool_Val i ->  Bool_Val (not i)
      | _ -> raise TypeError)

    | Lt (e1, e2) -> 
      let r1 = eval_expr e1 env in
      let r2 = eval_expr e2 env in
      (match r1, r2 with 
      | Int_Val i, Int_Val j ->  
        if i < j then Bool_Val true 
        else Bool_Val false
      | _, _ -> raise TypeError)

    | Leq (e1, e2) -> 
      let r1 = eval_expr e1 env in
      let r2 = eval_expr e2 env in
      (match r1, r2 with 
      | Int_Val i, Int_Val j ->  
        if i <= j then Bool_Val true 
        else Bool_Val false
      | _, _ -> raise TypeError)

    | Eq (e1, e2) -> 
      let r1 = eval_expr e1 env in
      let r2 = eval_expr e2 env in
      (match r1, r2 with 
      | Int_Val i, Int_Val j -> 
        if i = j then Bool_Val true 
        else Bool_Val false
      | Bool_Val i, Bool_Val j ->
        if i = j then Bool_Val true
        else Bool_Val false
      | _, _ -> raise TypeError)

    | Fun (p1, e1) -> 
      Closure (env, p1, e1)
    
    | App (e1, e2) -> 
      let r1 = eval_expr e1 env in
      let r2 = eval_expr e2 env in
      (match r1, r2 with 
      | Closure (evn, x, e), v -> 
        let evn_x = (x, v) :: evn in 
        eval_expr e evn_x
      | _, _ -> raise TypeError);;

(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
    match c with 
    | Skip -> 
      env

    | Comp (c1, c2) -> 
      let evn_c1 = eval_command c1 env in
        eval_command c2 evn_c1

    | Declare (d1, s1) ->
      (match d1 with 
      | Int_Type -> (s1, Int_Val 0) :: env
      | Bool_Type -> (s1, Bool_Val false) :: env
      | Lambda_Type -> (s1, Closure (env, "x", Var "x")) :: env)

    | Assg (s1, e1) -> 
      let r1 = eval_expr e1 env in
      (match r1 with
      | Int_Val i -> 
        (match List.assoc_opt s1 env with
        | None -> raise UndefinedVar
        | Some v -> 
          match v with 
          | Int_Val j -> (s1, r1) :: env
          | _ -> raise TypeError)
      | Bool_Val b -> 
        (match List.assoc_opt s1 env with
        | None -> raise UndefinedVar
        | Some v -> 
          match v with 
          | Bool_Val c -> (s1, r1) :: env
          | _ -> raise TypeError)
      
      | Closure (evn_r, x_r, e_r) -> 
        (match List.assoc_opt s1 env with
        | None -> raise UndefinedVar
        | Some v -> 
          match v with 
          | Closure (evn_v, x_v, e_v) -> (s1, r1) :: env
          | _ -> raise TypeError))

    | Cond (ge1, c1, c2) ->
      (match eval_expr ge1 env with 
      | Bool_Val true -> eval_command c1 env
      | Bool_Val false -> eval_command c2 env
      | _ -> raise TypeError)
      
    | While (ge1, c1) -> 
      (match eval_expr ge1 env with
      | Bool_Val true -> eval_command c (eval_command c1 env)
      | Bool_Val false -> env
      | _ -> raise TypeError) 

    | For (ge1 , c1) -> 
      (match eval_expr ge1 env with
      | Int_Val i when i <= 0 -> env
      | Int_Val j -> 
        let rec fold_n n f acc = 
          if n <= 0 then acc 
          else fold_n (n - 1) f (f c1 acc)
        in fold_n j eval_command env
      | _ -> raise TypeError);; 