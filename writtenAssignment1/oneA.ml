
open List

let divisible n l = 
  List.map (fun x -> if (x mod n) = 0 then true else false) l;;

let apply x f = f x;;

let f acc hd = 
  match acc with 
  | (lst, sum) -> (hd :: lst, sum + hd);;

(* derivation 

  x:6; x -> 6  
  -----------------
  x:3; x -> 3 
  x:3; 2 -> 2
  3 + 2 is 5
  ------------------
  3 -> 3
  x:3; x + 2 -> 5             5 != 0 
  -------------------------------------------------
  eq0 x:3; x + 2 -> false          x:6; 6 -> 6  
  -------------------------------------------------
  let x = 6 in if eq0 (let x = 3 in x + 2) then 3 else x -> 6 

*)

(* 

  5. (λx.x (λx.y x)) (λz.z)

    (λx.x (λx.y x)) (λz.z)
    λz.z (λx.y x)
    λx.y x

    beta reduction


  6. (λx.λy.x y z) (λc.c) ((λa.a) b) 

    (λx.λy.x y z) (λc.c) ((λa.a) b)
    (λy.λc.c y z) ((λa.a) b)
    (λy.λc.c y z) b
    λc.c b z
    b z 
    
    beta reduction

  7. (λx.(λy.(x y))) y

    (λx.(λy.(x y))) y
    (λx.(λz.(x z))) y - alpha reduction 
    (λz.(y z))        - beta reduction
    λz.y z

  

*)





