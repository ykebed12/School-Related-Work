(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = match tup with (a, b, c) -> (c, b, a);;

(*helper method for max_tup*)
let max a b = if a > b then a else b;;

let max_tup tup = match tup with (a, b, c) -> (max (max a b) c);;

let abs x = if x > 0 then x else x * (-1);;

let area x y = (fun = (a * b)) x y;;

let volume x y = failwith "unimplemented"

let equiv_frac (a, b) (x, y) = failwith "unimplemented"

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec factorial x = failwith "unimplemented"

let rec pow x = failwith "unimplemented"

let rec tail x num = failwith "unimplemented"

let rec log x y = failwith "unimplemented"

let rec len x = failwith "unimplemented"

let rec contains sub x = failwith "unimplemented"

(*********************)
(* Part 3: Variables *)
(*********************)

type lookup_table = unit

let empty_table () : lookup_table = failwith "unimplemented"

let push_scope (table:lookup_table) : lookup_table = failwith "unimplemented"

let pop_scope (table:lookup_table) : lookup_table = failwith "unimplemented"

let add_var name value (table:lookup_table) : lookup_table = failwith "unimplemented"

let rec lookup name (table:lookup_table) = failwith "unimplemented"
