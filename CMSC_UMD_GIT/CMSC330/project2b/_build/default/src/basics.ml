(*********************)
(* Part 3: Variables *)
(*********************)

type lookup_table = (string * int) list list;;

let empty_table () : lookup_table = [];;

let push_scope (table:lookup_table) : lookup_table = []::table;;

let pop_scope (table:lookup_table) : lookup_table = 
  match table with
  | [] -> failwith "No scopes remain!" 
  | h::t -> t
;;

let add_var (name:string) (value:int) (table:lookup_table) : lookup_table =
  let add_helper lst k v = (k,v)::lst in
    
    match table with
    | [] -> failwith "There are no scopes to add a variable to!"
    | h::t -> (add_helper h name value)::t

let rec lookup (name:string) (table:lookup_table) =
  let rec check_list n lst = 
    match lst with
    | [] -> false
    | tup::t -> if ((fun (a,b) -> a) tup) = n then true else check_list n t in

    let rec find_num n lst = 
      match lst with
      [] -> 0
      |(a,b)::t -> if a = name then b else find_num n t in 

      match table with
      | [] -> failwith "Variable not found!"
      | h::t -> if check_list name h then find_num name h else lookup name t
      ;;
 