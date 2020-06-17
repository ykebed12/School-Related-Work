open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let length lst = 
  fold (fun a _ -> a + 1) 0 lst

let helper_split (a,b,flag,pivot) x=
  if pivot = x then (a,b,true,pivot)
  else if flag = false then (a@[x],b,flag,pivot)
  else (a,b@[x],flag,pivot)
;;


let split_on pivot lst =
  let a = ([],[],false,pivot) in
    (fun (a,b,flag,pivot) -> (a,b)) (fold helper_split a lst)
;;

let flat_pair lst = 
  fold (fun acc (a,b) -> (acc@[a])@[b]) [] lst
;;

let comp fns arg = 
  fold_right (fun f acc -> (f acc)) fns arg

let ap fns args = 
  fold (fun acc fn-> acc@(map fn args) ) [] fns;;
