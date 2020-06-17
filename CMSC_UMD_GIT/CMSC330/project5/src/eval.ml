open SmallCTypes
open EvalUtils
open Builtins

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let assoc_opt k t =
  if List.mem_assoc k t then
    Some (List.assoc k t)
  else
    None

(* Insert value into environment and raise Declare error with passed message *)
let insert_val (k: string) (v: value) (env: environment) (msg: string): environment =
  ( 
    if List.mem_assoc k env then
      raise (DeclareError msg)
    else
      (k,v)::env
  )


(* Get value from environment and raise DeclareEror if it is not found. *)
let get_val env k = 
  (
    if List.mem_assoc k env then
      List.assoc k env
    else
      raise (DeclareError ("get_val"))
  )

(* Function references *)
let funcs = ref []

(* Used to access functions outside module. *)
let get_funcs () = !funcs

let reset_funcs () = funcs := []

let add_func name typ params body =
  if (List.mem_assoc name !funcs) then
    raise (DeclareError ("Function " ^ name ^" already exists"))
  else
    funcs := (name, (typ, params, body))::!funcs

let get_func name = List.assoc name !funcs

let get_type: (value -> data_type) = function
  |Thunk_Val (_, _, thunk_type) -> thunk_type
  |Int_Val (_) -> Int_Type
  |Bool_Val (_) -> Bool_Type

(* Helper function useful for inserting primitive values into the environment *)
let insert_primitive k v typ env =
  (
    match v with
    |Thunk_Val (_) -> raise (DeclareError "insert_primitive")
    |_ -> 
      if get_type v = typ then
        (
          if List.mem_assoc k env then
            raise (DeclareError "insert_primitive")
          else
            (k,v)::env
        )
      else
        raise (DeclareError "insert_primitive")

  )

let is_thunk (env: environment) (id: string): bool = 
  (
    if List.mem_assoc id env then
      (
        match List.assoc id env with
        |Thunk_Val (_) -> true
        |_ -> false
      )
    else 
      false
  )

(* checks that value equals expected data_type fails with TypeError otherwise *)
let check_type (expect: data_type) (v: value): value = 
  (
    if (get_type v) = expect then 
      v
    else
      raise (TypeError "Not same type")
  )



(* Creates thunks for each argument and inserts them into new environment used to evaluate function body.
    Fails if too many or too few arguments and "if" parameters. *)
let rec new_func_scope (params: parameter list) (args: expr list) (curr_env: environment): environment =
  try 
    (
      List.fold_left2
        (
          fun new_env (p_name, p_value) h_args ->
            (
              insert_val
                p_name 
                (Thunk_Val (curr_env, h_args, p_value))
                (new_env)
                ("new_func_sope")
            )
        )
        []
        params
        args
    )
  with
  |Invalid_argument _-> raise (TypeError "Different lengths of parameters and args")

(* Functions are mutually recursive *)
(* Replace this with your code from P4 and add to it to complete this project *)
let rec eval_expr env t =
  (
    match t with
    |Int x-> Int_Val x
    |Bool x-> Bool_Val x
    |ID x-> 
      (
        (*Change this*)
        match get_val env x with
        |Thunk_Val (env_t, expr_t, type_t) -> check_type type_t (eval_expr env_t expr_t)
        |Int_Val (y) -> Int_Val (y)
        |Bool_Val (y) -> Bool_Val (y)
      )
    |Add (expr1, expr2)->
      (
        match (evaluate (eval_expr env expr1), evaluate (eval_expr env expr2)) with
        |(Int_Val x, Int_Val y) -> (Int_Val (x + y))
        |_-> raise (TypeError "Type Error: Ints expected")
      )
    |Sub (expr1, expr2)->
      (
        match (evaluate (eval_expr env expr1), evaluate (eval_expr env expr2)) with
        |(Int_Val x, Int_Val y) -> (Int_Val (x - y))
        |_-> raise (TypeError "Type Error: Int expected")
      )
    |Mult (expr1, expr2)->
      (
        match (evaluate (eval_expr env expr1), evaluate (eval_expr env expr2)) with
        |(Int_Val x, Int_Val y)-> (Int_Val (x * y))
        |_-> raise (TypeError "Type Error: Int expected")
      )
    |Div (expr1, expr2)->
      (
        match (evaluate (eval_expr env expr1), evaluate (eval_expr env expr2)) with
        |(Int_Val x, Int_Val y) -> 
          if y = 0 then
            raise (DivByZeroError)
          else
            (Int_Val (x/y))
        |_-> raise (TypeError "Type Error: Int expected")
      )
    |Pow (expr1, expr2)->
      (
        match (evaluate (eval_expr env expr1), evaluate (eval_expr env expr2)) with
        |(Int_Val x, Int_Val y) -> 
            let float_ans = ((float_of_int x) ** (float_of_int y)) in
            (Int_Val (int_of_float float_ans))
        |_-> raise (TypeError "Type Error: Int expected")
      )
    |Or (expr1, expr2)->
      (
        match (evaluate (eval_expr env expr1), evaluate (eval_expr env expr2)) with
        |(Bool_Val x, Bool_Val y)-> Bool_Val (x || y)
        |_-> raise (TypeError "Type error: Bool expected")       
      )
    |And (expr1, expr2)->
      (
        match (evaluate (eval_expr env expr1), evaluate (eval_expr env expr2)) with
        |(Bool_Val x, Bool_Val y)-> Bool_Val (x && y)
        |_-> raise (TypeError "Type error: Bool expected")
      )
    |Not expr1->
      ( 
        (*Change this*)
        match evaluate (eval_expr env expr1) with
        |Bool_Val x-> Bool_Val (not x)
        |_ -> raise (TypeError "Type error Expr1: Bool expected")
      )
    |Greater (expr1, expr2) -> 
      ( 
        (*Change this*)
        match evaluate (eval_expr env expr1) with
        |Int_Val x-> 
          (
            match evaluate (eval_expr env expr2) with
            |Int_Val y-> Bool_Val (x > y)
            |Bool_Val y-> raise (TypeError "Type error Expr2: Int expected")
          )
        |Bool_Val x-> raise (TypeError "Type error Expr1: Int expected")
          
      )
    |Less (expr1, expr2) -> 
      ( 
        match evaluate (eval_expr env expr1) with
        |Int_Val x-> 
          (
            match evaluate (eval_expr env expr2) with
            |Int_Val y-> Bool_Val (x < y)
            |Bool_Val y-> raise (TypeError "Type error Expr2: Int expected")
          )
        |Bool_Val x-> raise (TypeError "Type error Expr1: Int expected")
          
      )
    |GreaterEqual (expr1, expr2) -> 
      ( 
        match evaluate (eval_expr env expr1) with
        |Int_Val x-> 
          (
            match evaluate (eval_expr env expr2) with
            |Int_Val y-> Bool_Val (x >= y)
            |Bool_Val y-> raise (TypeError "Type error Expr2: Int expected")
          )
        |Bool_Val x-> raise (TypeError "Type error Expr1: Int expected")
          
      )
    |LessEqual (expr1, expr2) -> 
      ( 
        match evaluate (eval_expr env expr1) with
        |Int_Val x-> 
          (
            match evaluate (eval_expr env expr2) with
            |Int_Val y-> Bool_Val (x <= y)
            |Bool_Val y-> raise (TypeError "Type error Expr2: Int expected")
          )
        |Bool_Val x-> raise (TypeError "Type error Expr1: Int expected")
          
      )
    |Equal (expr1, expr2) -> 
      ( 
        match evaluate (eval_expr env expr1) with
        |Int_Val x-> 
          (
            match evaluate (eval_expr env expr2) with
            |Int_Val y-> Bool_Val (x = y)
            |Bool_Val y-> raise (TypeError "Type error Expr2: Int expected")
          )
        |Bool_Val x->
          (
            match evaluate (eval_expr env expr2) with
            |Int_Val y-> raise (TypeError "Type error Expr2: Bool expected")
            |Bool_Val y-> Bool_Val (x = y)
          )
      )
    |NotEqual (expr1, expr2) -> 
      ( 
        match evaluate (eval_expr env expr1) with
        |Int_Val x-> 
          (
            match evaluate (eval_expr env expr2) with
            |Int_Val y-> Bool_Val (x <> y)
            |Bool_Val y-> raise (TypeError "Type error Expr2: Int expected")
          )
        |Bool_Val x->
          (
            match evaluate (eval_expr env expr2) with
            |Int_Val y-> raise (TypeError "Type error Expr2: Bool expected")
            |Bool_Val y-> Bool_Val (x <> y)
          )
      )
    |FunctionCall (name, args) ->
      let (type_f, parameters, code) = (get_func name) in
        (
          check_type type_f (
            get_val
            (eval_stmt (new_func_scope parameters args env) code)
            "~ret"
          )
        )
  )

and evaluate v =
  (
    match v with
    |Thunk_Val (env_t, expr_t, type_t) -> 
      check_type type_t (evaluate (eval_expr env_t expr_t))
    |_ -> v
  )

(* Replace this with your code from P4 and add to it to complete this project *)
and eval_stmt env s =
  if List.mem_assoc "~ret" env then
    env
  else
    (
      match s with
      |NoOp -> env
      |Seq (stmt1, stmt2) ->
        (
          let env' = eval_stmt env stmt1 in
          (eval_stmt env' stmt2)
        )
      |Declare (data_type, name)->
        (
          if (List.mem_assoc name env) then
            raise (DeclareError "Already declared variable")
          else
            (
              match data_type with
              |Int_Type-> (name, Int_Val (0))::env
              |Bool_Type-> (name, Bool_Val (false))::env
            )
        )
      |Assign (name, expr)->
        (
          (*Change this*)
          let evaluated_expr = evaluate(eval_expr env expr) in
          if List.mem_assoc name env then
            (
              let env_val = List.assoc name env in 
              match (env_val, evaluated_expr) with
              | (Int_Val (_), Int_Val (_)) -> (name, evaluated_expr)::env
              | (Bool_Val (_), Bool_Val (_)) -> (name, evaluated_expr)::env
              | _ -> raise (DeclareError ("Assign: Different types"))
            )
          else 
            raise (DeclareError ("Assign: Variable does not exist"))
        )
      |If (expr, stmt1, stmt2)->
        (
          match evaluate (eval_expr env expr) with
          |Bool_Val x -> 
            (
              if x then
                (eval_stmt env stmt1)
              else
                (eval_stmt env stmt2)
            )
          |_-> raise (TypeError "Type error: Bool expected")
        )
      |For (name, expr_start, expr_end, code)->
        (
          (*change this*)
          let start = evaluate (eval_expr env expr_start) in
          let finish = evaluate (eval_expr env expr_end) in

          match (start, finish) with
          | Int_Val(start_int), Int_Val(finish_int) ->
            if start_int <= finish_int then 
              (
                let new_env = eval_stmt ((name, Int_Val(start_int))::env) code in
                eval_stmt 
                  new_env 
                  ( 
                    Seq 
                      (
                        Assign(name, Add(ID name, Int (1))),
                        For(name, ID name, expr_end, code)
                      )
                  )
              )
            else
              env
          | _ -> raise (TypeError "For loop: Expected int")

        )
          
      |While (expr, stmt)->
        (
          match evaluate (eval_expr env expr) with
          |Bool_Val true-> 
            (
              let new_env = eval_stmt env stmt in
              eval_stmt new_env (While (expr,stmt))
            )
          |Bool_Val false->
            env
          |_-> raise (TypeError "TypeError: Boolean expected")
        )
      |Print expr -> 
        (
          match eval_expr env expr with
          |Int_Val x -> let _ = (print_output_int x) in let _= print_output_newline () in env
          |Bool_Val x -> let _ = (print_output_bool x) in let _= print_output_newline () in env   
        )
      |FunctionDecl (name , type_f, parameters, code) ->
        (
          if name ="main" then
            eval_stmt env code
          else
            (
              (add_func name type_f parameters code);
              env
            )
        )
      |Return expr ->
        ("~ret", evaluate (eval_expr env expr))::env
    )
