open List

let count123 l =
  let rec search (a, b, c) lst = 
    match lst with 
    | [] -> (a, b, c)
    | 1 :: t -> search (a + 1, b, c) t 
    | 2 :: t -> search (a, b + 1, c) t 
    | 3 :: t -> search (a, b, c + 1) t 
    | _ :: t -> search (a, b, c) t 
    in search (0, 0, 0) l;;

let rec n_times (f, n, v) =
  if n <= 0 then
    v
  else 
    n_times (f, n - 1, f v);;

let rec bucket_helper_append p va l = 
  match l with 
  | [] -> [[va]] (* create new list if empty or no eq match *)
  | x :: xs -> 
    match x with 
    | h :: t -> 
      match p va h with 
      | true -> ((x @ [va]) :: xs)
      | false -> (x :: (bucket_helper_append p va xs))

let buckets p l = 
  let rec fold f acc lst = 
    match lst with 
    | [] -> acc 
    | h :: t -> fold f (f p h acc) t in
  fold bucket_helper_append [] l;;

let fib_tailrec n =
  let rec aux i prev curr = 
    if i = 0 then prev
    else if i = 1 then curr
    else aux (i - 1) (curr) (prev + curr) in
  aux n 0 1;;
  
let assoc_list lst = 
  let get_freq_tuple lst x =
    List.fold_left (fun acc y -> if x = y then acc + 1 else acc) 0 lst in 
    let tuple_exists t_lst x =
      List.fold_left (fun acc (a, b) -> if a = x then acc + 1 else acc) 0 t_lst in 
      let insert_tuple v t_lst lst = 
        if (tuple_exists t_lst v) = 0 then (v, get_freq_tuple lst v) :: t_lst else t_lst in 
  List.fold_left (fun acc x -> insert_tuple x acc lst) [] lst;;
  
let ap fs args = 
  let lst = List.map (fun x -> List.map x args) fs in 
    List.fold_left (fun acc x -> acc @ x) [] lst;;

let maxl2 lst = 
  let maxl2_value = 
    let pair = 
      List.fold_left (fun (a, b) x -> 
        if x > a then (x, a)
        else if x > b then (a, x)
        else (a, b)) ((-1), (-1)) lst in 
    match pair with
    | (a, b) -> a + b in
  if maxl2_value <= 0 then 0 else maxl2_value;;
  
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec insert tree x =
  match tree with
  | Leaf -> Node(Leaf, x, Leaf)
  | Node(l, y, r) ->
     if x = y then tree
     else if x < y then Node(insert l x, y, r)
     else Node(l, y, insert r x)

let construct l =
  List.fold_left (fun acc x -> insert acc x) Leaf l

let rec fold_inorder f acc t =
  match t with 
  | Leaf -> acc
  | Node (l, v, r) -> fold_inorder f (f (fold_inorder f acc l) v) r;;

let levelOrder t = 
  let rec insert_level lst v lvl = 
    if (List.length lst) = lvl then 
      lst @ [[v]]
    else  
      List.mapi (fun i n_lst -> if i = lvl then n_lst @ [v] else n_lst) lst in 
    let rec fold_preorder f acc tr lv =
      match tr with
      | Leaf -> acc
      | Node (l, v, r) -> fold_preorder f (fold_preorder f (f acc v lv) l (lv + 1)) r (lv + 1) in
      fold_preorder insert_level [] t 0;; 
    
(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for count123 *)
  let _ =
    try
      assert (count123 [3;4;2;1;3] = (1,1,2));
      assert (count123 [4;4;1;2;1] = (2,1,0))
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for n_times *)
  let _ =
    try
      assert (n_times((fun x-> x+1), 50, 0) = 50);
      assert (n_times((fun x-> (x +. 2.0)), 50, 0.0) = 100.0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in
  
  (* Testcases for buckets *)
  let _ =
    try
      assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]);
      assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]);
      assert (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fib_tailrec *)
  let _ =
    try
      assert (fib_tailrec 50 = 12586269025);
      assert (fib_tailrec 90 = 2880067194370816120);
      assert (fib_tailrec 0 = 0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for assoc_list *)
  let _ =
    let y = ["a";"a";"b";"a"] in
    let z = [1;7;7;1;5;2;7;7] in
    let a = [true;false;false;true;false;false;false] in
    let b = [] in
    let cmp x y = if x < y then (-1) else if x = y then 0 else 1 in
    try
      assert ([("a",3);("b",1)] = List.sort cmp (assoc_list y));
      assert ([(1,2);(2,1);(5,1);(7,4)] = List.sort cmp (assoc_list z));
      assert ([(false,5);(true,2)] = List.sort cmp (assoc_list a));
      assert ([] = assoc_list b)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for ap *)
  let _ =
    let x = [5;6;7;3] in
    let b = [3] in
    let c = [] in
    let fs1 = [((+) 2) ; (( * ) 7)] in
    try
      assert  ([7;8;9;5;35;42;49;21] = ap fs1 x);
      assert  ([5;21] = ap fs1 b);
      assert  ([] = ap fs1 c);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in
  
  (* Testcases for maxl2 *)  
  let _ =
    try
      assert (maxl2 [1;10;2;100;3;400] = 500)
      ; assert (maxl2 [1] = 0)
      ; assert (maxl2 [] = 0)
      ; assert (maxl2 [1;10;2;100;3;100] = 200)
      ; assert (maxl2 [4;4;4;4;4;4] = 8)
      ; assert (maxl2 [1000;29;10;5;10000;100000] = 110000)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fold_inorder *)
  let _ =
    try
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]);
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Leaf, 1, Leaf)) = [1]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = 6)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for levelOrder *)
  let _ =
    try
      assert (levelOrder (construct [3;20;15;23;7;9]) = [[3];[20];[15;23];[7];[9]]);
      assert (levelOrder (construct [41;65;20;11;50;91;29;99;32;72]) = [[41];[20;65];[11;29;50;91];[32;72;99]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  if !error_count = 0 then  Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 9 programming questions are incorrect.\n") (!error_count)

let _ = main()
