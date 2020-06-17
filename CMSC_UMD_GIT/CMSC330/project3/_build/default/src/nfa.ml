open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let rec find_states ts_list state ts_symbol =
    match ts_list with
    | [] -> []
    | (q1,s,q2)::t when s = ts_symbol && q1 = state -> insert q2 (find_states t state ts_symbol)
    | _::t -> (find_states t state ts_symbol) in
  List.fold_left (fun acc h -> union (find_states nfa.delta h s) acc) [] qs
    ;;

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec f_helper vstd unvstd = 
    let new_unvstd = move nfa unvstd None in
    if subset new_unvstd vstd then union unvstd vstd else (f_helper (union vstd unvstd) new_unvstd) in
  f_helper [] qs
;;



(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

(*double check for dead states*)
(*Get e_closure of qs then make that first state, then get transition+e* states of first states*)
let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let move_states = List.fold_left (fun acc h -> (insert (move nfa qs (Some h)) acc)) [] nfa.sigma in
  (List.fold_left (fun acc h -> insert (e_closure nfa h) acc) [] move_states)
;;

(*Find new delta (Transitions)*)
let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left (fun acc h -> 
    insert
      (qs, Some h, (union (move nfa qs (Some h)) (e_closure nfa (move nfa qs (Some h))) ) ) 
      acc
  ) 
  []
  nfa.sigma
;;

(*Checks if qs is the final state in the dfa*)
let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if intersection qs nfa.fs = [] then [] else [qs]
;;


let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with
  | [] -> dfa
  | h::t ->
    let new_states' = new_states nfa h in
    let new_trans' = new_trans nfa h in
    let new_work = diff (union t new_states') dfa.qs in
    let dfa =
      {
        sigma = dfa.sigma;
        qs = insert h (union new_states' dfa.qs);
        q0 = dfa.q0;
        fs = dfa.fs;
        delta = union dfa.delta new_trans'
      }
    in
    nfa_to_dfa_step nfa dfa new_work
;;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = 
  let q0' = e_closure nfa [nfa.q0] in
  let qs' = [q0'] in
  let dfa =
  {
    sigma = nfa.sigma;
    qs = qs';
    q0 = q0';
    fs = [];
    delta = [];
  } in
  let dfa = nfa_to_dfa_step nfa dfa [q0'] in
  {
    sigma = nfa.sigma;
    qs = dfa.qs;
    q0 = dfa.q0;
    fs = List.fold_left (fun acc h -> 
      union (new_finals nfa h) acc    
    ) [] dfa.qs;
    delta = dfa.delta;
  }


;;


let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let dfa = nfa_to_dfa nfa in 
  let char_arr = explode s in
  let curr_state = e_closure nfa [nfa.q0] in
  let last_state = List.fold_left 
    (fun acc h -> 
      match (move dfa [acc] (Some h)) with
      |[] -> []
      |next_state::t -> next_state
    )
    curr_state
    char_arr 
    in

  if elem last_state dfa.fs then true else false
;;
