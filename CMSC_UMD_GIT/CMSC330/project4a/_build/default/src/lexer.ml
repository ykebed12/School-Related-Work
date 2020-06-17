open TokenTypes
open List


(*  Str.regexp Tok_Bool of bool*)
(*  Str.regexp Tok_Int of int*)
(*  Str.regexp Tok_ID of string*)

let re_whtspace = Str.regexp "[ \n\t]+";;
let re_id = Str.regexp "[a-zA-Z0-9]+";;
let re_boolt = Str.regexp "true";;
let re_boolf = Str.regexp "false";;
let re_int = Str.regexp "-?[0-9]+";;

let re_for = Str.regexp "for";;
let re_from = Str.regexp "from";;
let re_to = Str.regexp "to";;
let re_while = Str.regexp "while";;
let re_int_type = Str.regexp "int";;
let re_bool_type = Str.regexp "bool";;
let re_sub = Str.regexp "-";;
let re_semi = Str.regexp ";";;
let re_rparen = Str.regexp ")";;
let re_rbrace = Str.regexp "}";;
let re_printf = Str.regexp "printf";;
let re_pow = Str.regexp "\\^";;
let re_add = Str.regexp "\\+";;
let re_or = Str.regexp "||";;
let re_nequal = Str.regexp "!=";;
let re_not = Str.regexp "!";;
let re_mult = Str.regexp "\\*";;
let re_main = Str.regexp "main";;
let re_less_eq = Str.regexp "<=";;
let re_less = Str.regexp "<";;
let re_lparen = Str.regexp "(";;
let re_lbrace = Str.regexp "{";;
let re_if = Str.regexp "if";;
let re_great_eq = Str.regexp ">=";;
let re_great = Str.regexp ">";;
let re_equal = Str.regexp "==";;
let re_else = Str.regexp "else";;
let re_div = Str.regexp "\\/";;
let re_assign = Str.regexp "=";;
let re_and = Str.regexp "&&";;

let keywords = ["for";"from";"to";"while";"int";"bool";"printf";"main";"else";"if";"true";"false"];;

let tokenize input : token list =
  let rec tok pos str =
    if pos >= String.length str then
      [EOF]
    else
        (*check ints *)
      if Str.string_match re_int str pos then
        let matched_str = Str.matched_string str in
        let len = String.length matched_str in
        (Tok_Int (int_of_string matched_str))::(tok (pos + len) str)
        (*Check ID*)
      else if (Str.string_match re_id str pos) && (List.mem (Str.matched_string str) keywords) = false then
        let matched_str = Str.matched_string str in
        let len = String.length matched_str in
        (Tok_ID matched_str)::(tok (pos + len) str)
        (* check spaces*)
      else if Str.string_match re_whtspace str pos then
        let spaces = Str.matched_string str in
        let len = String.length spaces in
        (tok (pos + len) str)
      else if Str.string_match re_for str pos then
        Tok_For::(tok (pos + 3) str)
      else if Str.string_match re_from str pos then
        Tok_From::(tok (pos + 4) str)
      else if Str.string_match re_to str pos then
        Tok_To::(tok (pos + 2) str)
      else if Str.string_match re_while str pos then
        Tok_While::(tok (pos + 5) str)
      else if Str.string_match re_int_type str pos then
        Tok_Int_Type::(tok (pos + 3) str)
      else if Str.string_match re_bool_type str pos then
        Tok_Bool_Type::(tok (pos + 4) str)
      else if Str.string_match re_sub str pos then
        Tok_Sub::(tok (pos + 1) str)
      else if Str.string_match re_semi str pos then
        Tok_Semi::(tok (pos + 1) str)
      else if Str.string_match re_rparen str pos then
        Tok_RParen::(tok (pos + 1) str)
      else if Str.string_match re_rbrace str pos then
        Tok_RBrace::(tok (pos + 1) str)
      else if Str.string_match re_printf str pos then
        Tok_Print::(tok (pos + 6) str)
      else if Str.string_match re_pow str pos then
        Tok_Pow::(tok (pos + 1) str)
      else if Str.string_match re_add str pos then
        Tok_Add::(tok (pos + 1) str)
      else if Str.string_match re_or str pos then
        Tok_Or::(tok (pos + 2) str)
      else if Str.string_match re_nequal str pos then
        Tok_NotEqual::(tok (pos + 2) str)
      else if Str.string_match re_not str pos then
        Tok_Not::(tok (pos + 1) str)
      else if Str.string_match re_mult str pos then
        Tok_Mult::(tok (pos + 1) str)
      else if Str.string_match re_main str pos then
        Tok_Main::(tok (pos + 4) str)
      else if Str.string_match re_less_eq str pos then
        Tok_LessEqual::(tok (pos + 2) str)
      else if Str.string_match re_less str pos then
        Tok_Less::(tok (pos + 1) str)
      else if Str.string_match re_lparen str pos then
        Tok_LParen::(tok (pos + 1) str)
      else if Str.string_match re_lbrace str pos then
        Tok_LBrace::(tok (pos + 1) str)
      else if Str.string_match re_if str pos then
        Tok_If::(tok (pos + 2) str)
      else if Str.string_match re_great_eq str pos then
        Tok_GreaterEqual::(tok (pos + 2) str)
      else if Str.string_match re_great str pos then
        Tok_Greater::(tok (pos + 1) str)
      else if Str.string_match re_equal str pos then
        Tok_Equal::(tok (pos + 2) str)
      else if Str.string_match re_else str pos then
        Tok_Else::(tok (pos + 4) str)
      else if Str.string_match re_div str pos then
        Tok_Div::(tok (pos + 1) str)
      else if Str.string_match re_assign str pos then
        Tok_Assign::(tok (pos + 1) str )
      else if Str.string_match re_and str pos then
        Tok_And::(tok (pos + 2) str)
        (*Check bool*)
      else if Str.string_match re_boolt str pos then
        (Tok_Bool true)::(tok (pos + 4) str) 
      else if Str.string_match re_boolf str pos then
        (Tok_Bool false)::(tok (pos + 5) str)   
      else 
        raise (InvalidInputException "tokenize")
    in
  tok 0 input