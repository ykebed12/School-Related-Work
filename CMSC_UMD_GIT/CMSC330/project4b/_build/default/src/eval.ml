open SmallCTypes
open EvalUtils
open TokenTypes
open List

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec eval_expr env t =
  (
    match t with
    |Int x-> Int_Val x
    |Bool x-> Bool_Val x
    |ID x-> 
      (
        if exists env x then
          lookup env x
        else
          raise (DeclareError "ID not found")
      )
    |Add (expr1, expr2)->
      (
        match (eval_expr env expr1, eval_expr env expr2) with
        |(Int_Val x, Int_Val y) -> (Int_Val (x + y))
        |_-> raise (TypeError "Type Error: Ints expected")
      )
    |Sub (expr1, expr2)->
      (
        match (eval_expr env expr1, eval_expr env expr2) with
        |(Int_Val x, Int_Val y) -> (Int_Val (x - y))
        |_-> raise (TypeError "Type Error: Int expected")
      )
    |Mult (expr1, expr2)->
      (
        match (eval_expr env expr1, eval_expr env expr2) with
        |(Int_Val x, Int_Val y)-> (Int_Val (x * y))
        |_-> raise (TypeError "Type Error: Int expected")
      )
    |Div (expr1, expr2)->
      (
        match (eval_expr env expr1, eval_expr env expr2) with
        |(Int_Val x, Int_Val y) -> 
          if y = 0 then
            raise (DivByZeroError)
          else
            (Int_Val (x/y))
        |_-> raise (TypeError "Type Error: Int expected")
      )
    |Pow (expr1, expr2)->
      (
        match (eval_expr env expr1, eval_expr env expr2) with
        |(Int_Val x, Int_Val y) -> 
            let float_ans = ((float_of_int x) ** (float_of_int y)) in
            (Int_Val (int_of_float float_ans))
        |_-> raise (TypeError "Type Error: Int expected")
      )
    |Or (expr1, expr2)->
      (
        match (eval_expr env expr1, eval_expr env expr2) with
        |(Bool_Val x, Bool_Val y)-> Bool_Val (x || y)
        |_-> raise (TypeError "Type error: Bool expected")       
      )
    |And (expr1, expr2)->
      (
        match (eval_expr env expr1, eval_expr env expr2) with
        |(Bool_Val x, Bool_Val y)-> Bool_Val (x && y)
        |_-> raise (TypeError "Type error: Bool expected")
      )
    |Not expr1->
      ( 
        match eval_expr env expr1 with
        |Int_Val x-> raise (TypeError "Type error Expr1: Bool expected")
        |Bool_Val x-> Bool_Val (not x)
      )
    |Greater (expr1, expr2) -> 
      ( 
        match eval_expr env expr1 with
        |Int_Val x-> 
          (
            match eval_expr env expr2 with
            |Int_Val y-> Bool_Val (x > y)
            |Bool_Val y-> raise (TypeError "Type error Expr2: Int expected")
          )
        |Bool_Val x-> raise (TypeError "Type error Expr1: Int expected")
          
      )
    | Less (expr1, expr2) -> 
      ( 
        match eval_expr env expr1 with
        |Int_Val x-> 
          (
            match eval_expr env expr2 with
            |Int_Val y-> Bool_Val (x < y)
            |Bool_Val y-> raise (TypeError "Type error Expr2: Int expected")
          )
        |Bool_Val x-> raise (TypeError "Type error Expr1: Int expected")
          
      )
    | GreaterEqual (expr1, expr2) -> 
      ( 
        match eval_expr env expr1 with
        |Int_Val x-> 
          (
            match eval_expr env expr2 with
            |Int_Val y-> Bool_Val (x >= y)
            |Bool_Val y-> raise (TypeError "Type error Expr2: Int expected")
          )
        |Bool_Val x-> raise (TypeError "Type error Expr1: Int expected")
          
      )
    | LessEqual (expr1, expr2) -> 
      ( 
        match eval_expr env expr1 with
        |Int_Val x-> 
          (
            match eval_expr env expr2 with
            |Int_Val y-> Bool_Val (x <= y)
            |Bool_Val y-> raise (TypeError "Type error Expr2: Int expected")
          )
        |Bool_Val x-> raise (TypeError "Type error Expr1: Int expected")
          
      )
    | Equal (expr1, expr2) -> 
      ( 
        match eval_expr env expr1 with
        |Int_Val x-> 
          (
            match eval_expr env expr2 with
            |Int_Val y-> Bool_Val (x = y)
            |Bool_Val y-> raise (TypeError "Type error Expr2: Int expected")
          )
        |Bool_Val x->
          (
            match eval_expr env expr2 with
            |Int_Val y-> raise (TypeError "Type error Expr2: Bool expected")
            |Bool_Val y-> Bool_Val (x = y)
          )
      )
    | NotEqual (expr1, expr2) -> 
      ( 
        match eval_expr env expr1 with
        |Int_Val x-> 
          (
            match eval_expr env expr2 with
            |Int_Val y-> Bool_Val (x <> y)
            |Bool_Val y-> raise (TypeError "Type error Expr2: Int expected")
          )
        |Bool_Val x->
          (
            match eval_expr env expr2 with
            |Int_Val y-> raise (TypeError "Type error Expr2: Bool expected")
            |Bool_Val y-> Bool_Val (x <> y)
          )
      )
  
  )

and lookup env field =
  (
    match env with
    |[]-> failwith "Lookup not found"
    |(name, value)::t when field = name -> value
    |_::t-> lookup t field
  )

and exists env field =
  (
    match env with
    |[]-> false
    |(name, value)::t->
      if name = field then
        true
      else
        exists t field
  )
  
let rec eval_stmt env s =
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
        if exists env name then
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
        if exists env name then
          let val' = lookup env name in
          let new_val = eval_expr env expr in
          let new_var = 
          (
            match (val', new_val) with
            |(Int_Val x, Int_Val y) -> (name, new_val)
            |(Bool_Val x, Bool_Val y) -> (name, new_val)
            |_-> raise (TypeError "Type error")
          ) in 
          new_var::env

        else
          raise (DeclareError "Variable does not exist in environment")
      )
    |If (expr, stmt1, stmt2)->
      (
        match (eval_expr env expr) with
        |Bool_Val x -> 
          (
            if x then
              (eval_stmt env stmt1)
            else
              (eval_stmt env stmt2)
          )
        |_-> raise (TypeError "Type error: Bool expected")
      )
    |For (name, expr1, expr2, stmt)->
      (
        if exists env name then
          (
            match (lookup env name, eval_expr env expr1, eval_expr env expr2) with
            |(Int_Val curr_val, Int_Val x, Int_Val y) when x <= y -> 
              (
                let new_env = eval_stmt ((name, Int_Val(x))::env) stmt in
                let new_env' = eval_stmt new_env (Assign (name, Add(ID name, Int (1)))) in
                eval_stmt new_env' (For (name, ID name , Int y, stmt))
              )
            |(Int_Val curr_val, Int_Val x, Int_Val y) -> env
            |_-> raise (TypeError "Type error: Int expected")
          )
        else
          raise (TypeError "Type error: Name doesn't exists") 
      )
    |While (expr, stmt)->
      (
        match eval_expr env expr with
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
            
  )

