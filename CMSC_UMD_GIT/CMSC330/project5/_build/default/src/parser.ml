open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers *)

let tok_list = ref []

(* Returns next token in the list. *)
let lookahead () : token =
  match !tok_list with
  | [] -> raise (Failure "no tokens")
  | h :: t -> h

(* Matches the top token in the list. *)
let consume (a : token) : unit =
  match !tok_list with
  | h :: t when a = h -> tok_list := t
  | _ -> raise (Failure "bad match")

(* Parsing *)

let rec parse_expr () =
  parse_or ()

and parse_or () =
  (
    let expr1 = parse_and () in
    match lookahead () with
    |Tok_Or -> 
      (
        consume Tok_Or;
        let expr2 = parse_or () in
        (Or (expr1, expr2))
      )
    |_ -> expr1
  )

and parse_and () = 
  (
    let expr1 = parse_equal () in
    match lookahead () with
    |Tok_And->
      (
        consume Tok_And;
        let expr2 = parse_and () in
        (And (expr1, expr2))
      )
    |_-> expr1
  )

and parse_equal () = 
  (
    let expr1 = parse_relation () in
    match lookahead () with
    |Tok_Equal->
      (
        consume Tok_Equal;
        let expr2 = parse_equal () in
        (Equal (expr1, expr2))
      )
    |Tok_NotEqual->
      (
        consume Tok_NotEqual;
        let expr2 = parse_equal () in
        (NotEqual (expr1, expr2))
      )
    |_-> expr1
  )

and parse_relation () =
  (
    let expr1 = parse_add () in
    match lookahead () with
    |Tok_Less-> 
      (
        consume Tok_Less;
        let expr2 = parse_relation () in
        (Less (expr1, expr2))
      )
    |Tok_LessEqual->
      (
        consume Tok_LessEqual;
        let expr2 = parse_relation () in
        (LessEqual (expr1, expr2))
      )
    |Tok_Greater->
      (
        consume Tok_Greater;
        let expr2 = parse_relation () in
        (Greater (expr1, expr2))
      )
    |Tok_GreaterEqual->
      (
        consume Tok_GreaterEqual;
        let expr2 = parse_relation () in
        (GreaterEqual (expr1, expr2))
      )
    |_-> expr1;
  )

and parse_add () = 
  (
    let expr1 = parse_mult () in
    match lookahead () with
    |Tok_Add->
      (
        consume Tok_Add;
        let expr2 = parse_add () in
        (Add (expr1, expr2))
      )
    |Tok_Sub->
      (
        consume Tok_Sub;
        let expr2 = parse_add () in
        (Sub (expr1, expr2))
      )
    |_-> expr1
  )

and parse_mult () = 
  (
    let expr1 = parse_power () in
    match lookahead () with
    |Tok_Mult->
      (
        consume Tok_Mult;
        let expr2 = parse_mult () in
        (Mult (expr1, expr2))
      )
    |Tok_Div->
      (
        consume Tok_Div;
        let expr2 = parse_mult () in
        (Div (expr1, expr2))
      )
    |_-> expr1
  )

and parse_power () =
  (
    let expr1 = parse_unary () in
    match lookahead () with
    |Tok_Pow->
      (
        consume Tok_Pow;
        let expr2 = parse_power () in
        (Pow (expr1, expr2))
      )
    |_-> expr1
  )

and parse_unary () =
  (
    match lookahead () with
    |Tok_Not -> 
      (
        consume Tok_Not;
        let expr1 = parse_unary () in
        (Not (expr1))
      )
    |_-> parse_literal ()
  )

  (*Changed section for project 5*)
and parse_literal () = 
  (
    match lookahead () with
    |Tok_Int x-> 
      (
        consume (Tok_Int x);
        (Int (x))
      )
    |Tok_Bool x->
      (
        consume (Tok_Bool x);
        (Bool (x))
      )
    |Tok_ID x->
      (
        consume (Tok_ID x);
        if (lookahead ()) = Tok_LParen then 
          (
            consume Tok_LParen;
            let args = parse_args_call () in
            consume Tok_RParen;
            FunctionCall(x, args)
          )
        else
          (ID (x))
      )
    |Tok_LParen->
      (
        consume (Tok_LParen);
        let expr1 = parse_expr () in
        consume (Tok_RParen);
        expr1
      )
    |_->raise (InvalidInputException("parse_expr"))
  )

and parse_options () =
  ( 
    match lookahead () with
    |Tok_Int_Type ->
      (
        consume Tok_Int_Type;
        match lookahead () with
        |Tok_ID x->
          consume (Tok_ID x);
          consume Tok_Semi;
          (Declare (Int_Type, x))
        |_-> raise (InvalidInputException("Int"))
      )
    
    |Tok_Bool_Type -> 
      (
        consume Tok_Bool_Type;
        match lookahead () with
        |Tok_ID x->
          consume (Tok_ID x);
          consume Tok_Semi;
          (Declare (Bool_Type, x))
        |_-> raise (InvalidInputException("Bool"))
      )
    
    |Tok_ID x->
      (
        consume (Tok_ID x);
        consume Tok_Assign;
        let expr = parse_expr () in
        consume Tok_Semi;
        (Assign (x, expr))
      )

    |Tok_Print ->
      (
        consume Tok_Print;
        let exp = (parse_expr ()) in
        consume Tok_Semi;
        (Print (exp))
      )

    |Tok_If ->
      (
        consume Tok_If;
        let exp = (parse_expr ()) in
        consume Tok_LBrace;
        let l = (parse_stmt ()) in
        consume Tok_RBrace;
        
        match lookahead () with
        |Tok_Else ->
          (
            consume Tok_Else; 
            consume Tok_LBrace;
            let r = (parse_stmt ()) in
            consume Tok_RBrace;
            (If ( exp, l, r))
          )
        |_ -> 
          (If (exp, l, NoOp))
      )

    |Tok_While -> 
      (
        consume Tok_While;
        let exp = (parse_expr ()) in
        consume Tok_LBrace; 
        let new_stmt = (parse_stmt ()) in
        consume Tok_RBrace;
        (While (exp, new_stmt))
      )
    
    |Tok_For ->
      (
        consume Tok_For;
        consume Tok_LParen;
      
        match lookahead () with
        |Tok_ID start ->
          (
            consume (Tok_ID start);
            consume Tok_From;
            let l = parse_expr () in
            consume Tok_To;
            let r = parse_expr () in
            consume Tok_RParen;
            consume Tok_LBrace; 
            let statment = parse_stmt () in
            consume Tok_RBrace;
            For(start, l, r, statment)
          )
        |_ -> (raise (InvalidInputException("parse_stmt")))
      )
    |Tok_Return -> 
      (
        consume Tok_Return;
        let expr = parse_expr () in
        consume Tok_Semi;
        (Return (expr))
      )            
    |_ -> NoOp
  )

and parse_stmt () =
  (
    let l = (parse_options ()) in
    if l = NoOp then 
      NoOp
    else
      (Seq (l, parse_stmt ()))
  )

   (*Changed section for project 5*)
   (*This is for declaration*)
and parse_args_dec () =
  (
    let parse_args () = 
      (
        match lookahead () with
        |Tok_Int_Type -> 
          consume Tok_Int_Type;
          (
            match lookahead () with
            |Tok_ID x -> 
              (
                consume (Tok_ID x);
                (x, Int_Type)
              )
            |_ -> (raise (InvalidInputException "parse_args"))
          )
        |Tok_Bool_Type ->
          (
            consume Tok_Bool_Type;
            match lookahead () with
            |Tok_ID x -> 
              (
                consume (Tok_ID x);
                (x, Bool_Type)
              )
            | _ -> (raise (InvalidInputException "parse_args"))
          )
        |_ -> raise (InvalidInputException "parse_args")
      ) in 
    
    (* Actual function*)
    if (lookahead ()) = Tok_RParen then
      []
    else
      (
        let arg = (parse_args ()) in
          (
            if (lookahead ()) = Tok_Comma then
              (
                consume Tok_Comma;
                let nxt_args = parse_args_dec () in
                [arg] @ nxt_args
              )
            else 
              [arg]
          )
      )
      
  )

(*This is for funtion call*)
and parse_args_call () = 
  (
    let rec get_args lst =
    ( 
      match lookahead () with
      | Tok_RParen -> lst
      | _ -> 
          let arg = (parse_expr ()) in 
          (
            match lookahead () with
            | Tok_Comma -> 
              (
                consume Tok_Comma;
                (get_args (lst @ [arg]))
              )
            | _ -> (lst @ [arg])
          )
    ) in 
    get_args []
  )


and parse_function_sec () =
  (
    match lookahead () with
    |Tok_ID x ->
      (
        consume (Tok_ID x);
        consume Tok_LParen;
        let args = (parse_args_dec ()) in
        consume Tok_RParen;
        consume Tok_LBrace;
        let stmt_code = (parse_stmt ()) in
        consume Tok_RBrace;
        (args, stmt_code)
      )
    |_ -> (raise (InvalidInputException("consume_function")))
  )
  
and parse_main_func () =
  (
    consume Tok_Main;
    consume Tok_LParen;
    consume Tok_RParen;
    consume Tok_LBrace;
    let stmt_code = (parse_stmt ()) in
    consume Tok_RBrace;
    (FunctionDecl("main", Int_Type, [], stmt_code))
  )

and parse_first () = 
  (
    match lookahead () with
    |Tok_Int_Type ->
      (
        consume Tok_Int_Type;
        match lookahead () with
        |Tok_Main ->
          (
            parse_main_func ()
          )          
        |Tok_ID x ->
          (
            let (args, stmt_code) = (parse_function_sec ()) in
            FunctionDecl(x, Int_Type, args, stmt_code)
          )
        | _ -> (raise (InvalidInputException("parse_fun_dec")))
      )
    |Tok_Bool_Type -> 
      ( 
        consume Tok_Bool_Type;
        match lookahead () with
        | Tok_ID x ->
          (
            let (args, stmt_code) = parse_function_sec () in
            FunctionDecl(x, Bool_Type, args, stmt_code)
          )
        |_ -> (raise (InvalidInputException("parse_fun_dec")))
      )
    |_ -> NoOp
  )
;;

let rec parse_top_level toks =
  (
    tok_list :=toks;
    let rec parse_program () =
    (
      let first_func = (parse_first ()) in
      match lookahead () with
      | EOF -> 
        first_func
      | _ -> 
        (
          (Seq (first_func, parse_program ()))
        )
    ) in
    parse_program ()
  )
;;







(* PLEASE UNCOMMENT BASED ON YOUR IMPLEMENTATION *)
(* ONLY ONE LINE SHOULD BE UNCOMMENTED IN EACH FUNCTION *)
let parse_expr_wrapper toks =
    (* UNCOMMENT the following line if you did the parser FUNCTIONALLY *)
    (* let (_, e) = parse_expr toks in e *)
    (* UNCOMMENT the following line if you did the parser IMPERATIVELY *)
    tok_list := toks; parse_expr ()

let parse_stmt_wrapper toks =
    (* UNCOMMENT the following line if you did the parser FUNCTIONALLY *)
    (* let (_, e) = parse_stmt toks in e *)
    (* UNCOMMENT the following line if you did the parser IMPERATIVELY *)
    tok_list := toks; parse_stmt ()
