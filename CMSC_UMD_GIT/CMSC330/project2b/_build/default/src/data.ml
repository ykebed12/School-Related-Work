open Funs

(***********************)
(* Part 2: Integer BST *)
(***********************)

type int_tree =
  | IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf 

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode (x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l

(* Implement the functions below. *)

let rec int_insert_all lst t = 
  fold (fun acc x -> (int_insert x acc)) t lst;;

let rec int_depth t =
  match t with
  | IntLeaf -> 0
  | IntNode (y, l, r) -> 
    let left_depth = int_depth l in
    let right_depth = int_depth r in

    if left_depth > right_depth then left_depth + 1 else right_depth + 1
  ;;

let rec int_common t x y = 
  match t with
  |IntLeaf -> invalid_arg "int_common"
  |IntNode (d,l,r) when (int_mem x t && int_mem y t)-> 
    if d < x && d < y
      then int_common r x y
    else if d > x && d > y
      then int_common l x y
    else d
  |IntNode (d,l,r) -> invalid_arg "int_common"
;;

let rec int_level lvl t = 
  match t with
  |IntLeaf -> []
  |IntNode (y, l, r) when lvl > 0 -> 
    (int_level (lvl - 1) l)@(int_level (lvl - 1) r)
  |IntNode (y, l, r) when lvl = 0 -> [y]
  |IntNode (y, l, r) -> []
;;

(***************************)
(* Part 3: Polymorphic BST *)
(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec pinsert_helper f x t =
  match t with
    Leaf -> Node (x, Leaf, Leaf)
  | Node (y, l, r) when (f x y) > 0 -> Node (y, l, (pinsert_helper f x r))
  | Node (y, l, r) when (f x y) < 0 -> Node (y, (pinsert_helper f x l), r)
  | Node (y, l, r) -> t
  

let rec pinsert x t = 
  match t with
    (f,tree) -> (f,(pinsert_helper f x tree))
;;

let rec pmem_helper f x t = 
  match t with
    Leaf -> false
  | Node (y, l, r) when (f x y) > 0 -> pmem_helper f x r
  | Node (y, l, r) when (f x y) = 0 -> true
  | Node (y, l, r) -> pmem_helper f x l
;;

let pmem x t =
  match t with
    (f,tree) -> pmem_helper f x tree
;;


let pinsert_all lst t = 
  fold (fun tree x -> pinsert x tree) t lst;;
;;

let rec plist_help t =
  match t with
  | Leaf -> []
  | Node (y, l, r) -> 
    let l_tree = plist_help l in
    let r_tree = plist_help r in
    l_tree @ [y] @ r_tree
;;


let rec p_as_list t = 
  match t with
    (f,tree) -> plist_help tree
;;

let pmap f t = 
  let t_fn = (fun (fn,tree) -> fn) t in
  let t_lst = p_as_list t in
  let lst_map = map f t_lst in
  pinsert_all lst_map (empty_ptree t_fn)



(********************************)
(* Part 4: Graphs with Records *)
(*******************************)

type node = int
type edge = { src : node; dst : node; }
type graph = { nodes : int_tree; edges : edge list; }

let empty_graph = {nodes = empty_int_tree; edges = [] }

let add_edge e { nodes = ns; edges = es } =
    let { src = s; dst = d } = e in
    let ns' = int_insert s ns in
    let ns'' = int_insert d ns' in
    let es' = e::es in
    { nodes = ns''; edges = es' }

let add_edges es g = fold (fun g e -> add_edge e g) g es

(* Implement the functions below. *)

let graph_empty g = 
  match g with
  | {nodes = empty_int_tree; edges = [] } -> true
  | graph -> false
;;
let graph_size g = 
  match g with
  | {nodes = n; edges = _ } -> int_depth n
;;

let is_dst n e = 
  match e with
  { src = _; dst = e_node} -> e_node = n
;;

let src_edges n g =
  let find_edges a {src = src_n; dst = dst} = 
    if src_n = n 
      then {src = src_n; dst = dst}::a
    else a in

  match g with
  {nodes = _; edges = edge_list} ->
    fold find_edges [] edge_list
;;

let rec reachable_nodes n g tree  = 
  (*find edges with src n and if the dst is not in the tree
    recursively put dst in the place of n and add the dst to the tree *)
  let edge_lst = src_edges n g in 
  fold (fun acc {src = s; dst = d} -> 
    if int_mem d acc then acc 
    else reachable_nodes d g (int_insert d acc)) tree edge_lst
;;

let reachable n g =
  let {nodes = node_tree; edges = edge_lst} = g in
  if int_mem n node_tree 
    then int_insert n (reachable_nodes n g empty_int_tree)
  else empty_int_tree
;;
  
  


  

