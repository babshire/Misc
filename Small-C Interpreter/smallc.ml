(*  
	CMSC330 Fall 2016
	This ocaml code reads a C code and properly indents it
	
	compile for debug:
		ocamlc -g Str.cma smallc.ml 
	
	@author: Anwar Mamat
	@date: 10/15/2016
*)

#load "str.cma"

type data_type =
	|Type_Int
;;

(* Use this as your abstract syntax tree *)

type ast =
  | Id of string
  | Num of int
  | Define of data_type * ast
  | Assign of ast * ast
  | List of ast list
  | Fun of data_type * string * ast * ast   (* return type * function name * argument list * statement list *)
  | Sum of ast * ast
  | Greater of ast * ast
  | Equal of ast * ast
  | Less of ast * ast
  | Mult of ast * ast
  | Pow of  ast * ast
  | Print of ast
  | If of ast * ast * ast	(* cond * if brach * else branch *)
  | While of ast * ast
  | Paren of ast
  
;;

type token =
 | Tok_Id of string
 | Tok_Num of int
 | Tok_String of string
 | Tok_Assign
 | Tok_Greater
 | Tok_Less
 | Tok_Equal
 | Tok_LParen
 | Tok_RParen
 | Tok_Semi
 | Tok_Main
 | Tok_LBrace
 | Tok_RBrace
 | Tok_Int 
 | Tok_Float
 | Tok_Sum
 | Tok_Mult
 | Tok_Pow
 | Tok_Print
 | Tok_If
 | Tok_Else
 | Tok_While
 | Tok_END
 
(* tokens *)
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_lbrace = Str.regexp "{"
let re_rbrace = Str.regexp "}"
let re_assign = Str.regexp "="
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_equal = Str.regexp "=="
let re_semi = Str.regexp ";"
let re_int = Str.regexp "int"
let re_float = Str.regexp "float"
let re_printf = Str.regexp "printf"
let re_main = Str.regexp "main"
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_num = Str.regexp "[-]?[0-9]+"
let re_string = Str.regexp "\"[^\"]*\""
let re_whitespace = Str.regexp "[ \t\n]"
let re_add = Str.regexp "+"
let re_mult = Str.regexp "*"
let re_pow = Str.regexp "\\^"
let re_if = Str.regexp "if"
let re_else = Str.regexp "else"
let re_while = Str.regexp "while"


exception Lex_error of int
exception Parse_error of int ;;
exception IllegalExpression of string

let tokenize s =
 let rec tokenize' pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_add s pos) then
       Tok_Sum::(tokenize' (pos+1) s)
     else if (Str.string_match re_mult s pos) then
       Tok_Mult::(tokenize' (pos+1) s)
     else if (Str.string_match re_equal s pos) then
       Tok_Equal::(tokenize' (pos+2) s)
     else if (Str.string_match re_if s pos) then
       Tok_If::(tokenize' (pos+2) s)
     else if (Str.string_match re_else s pos) then
       Tok_Else::(tokenize' (pos+4) s)    
     else if (Str.string_match re_while s pos) then
       Tok_While::(tokenize' (pos+5) s)       
	else if (Str.string_match re_pow s pos) then
       Tok_Pow::(tokenize' (pos+1) s)
    else if (Str.string_match re_printf s pos) then
       Tok_Print::tokenize' (pos+6) s
    else if (Str.string_match re_lbrace s pos) then
       Tok_LBrace::(tokenize' (pos+1) s)
    else if (Str.string_match re_rbrace s pos) then
       Tok_RBrace::(tokenize' (pos+1) s)
    else if (Str.string_match re_assign s pos) then
       Tok_Assign::(tokenize' (pos+1) s)
    else if (Str.string_match re_greater s pos) then
       Tok_Greater::(tokenize' (pos+1) s)
    else if (Str.string_match re_less s pos) then
       Tok_Less::(tokenize' (pos+1) s)
    else if (Str.string_match re_semi s pos) then
       Tok_Semi::(tokenize' (pos+1) s)
    else if (Str.string_match re_int s pos) then
       Tok_Int::(tokenize' (pos+3) s)
    else if (Str.string_match re_float s pos) then
       Tok_Float::(tokenize' (pos+5) s)
    else if (Str.string_match re_main s pos) then
       Tok_Main::(tokenize' (pos+4) s)
     else if (Str.string_match re_id s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Id token)::(tokenize' new_pos s)
     else if (Str.string_match re_string s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       let tok = Tok_String (String.sub token 1 ((String.length token)-2)) in
       tok::(tokenize' new_pos s)
     else if (Str.string_match re_num s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Num (int_of_string token))::(tokenize' new_pos s)
     else if (Str.string_match re_whitespace s pos) then
       tokenize' (Str.match_end ()) s
     else
       raise (Lex_error pos)
   end
 in
 tokenize' 0 s
 
 
 (* C Grammar *)
 (* 
 
 basicType-> 'int'
  mainMethod-> basicType 'main' '(' ')' '{' methodBody '}'
  methodBody->(localDeclaration | statement)*
  localDeclaration->basicType ID ';'
  statement->
    whileStatement
    |ifStatement
    |assignStatement
    |printStatement
  
  assignStatement->ID '=' exp ';'
  ifStatement -> 'if' '(' exp ')'  '{' ( statement)* '}'  ( 'else' '{'( statement)* '}')?
  whileStatement -> 'while''(' exp ')' '{'(statement )*'}'
  printStatement->'printf' '(' exp ')' ';'
  exp -> additiveExp (('>'  | '<'  | '==' ) additiveExp )*
  additiveExp -> multiplicativeExp ('+' multiplicativeExp)*
  multiplicativeExp-> powerExp ( '*' powerExp  )*
  powerExp->primaryExp ( '^' primaryExp) *
  primaryExp->'(' exp ')' | ID 
  ID->( 'a'..'z' | 'A'..'Z') ( 'a'..'z' | 'A'..'Z' | '0'..'9')*
  WS-> (' '|'\r'|'\t'|'\n') 



*)

(*----------------------------------------------------------
  function lookahead : token list -> (token * token list)
	Returns tuple of head of token list & tail of token list
*)

let lookahead tok_list = match tok_list with
        [] -> raise (IllegalExpression "lookahead")
        | (h::t) -> (h,t)
;;        



(* -------------- Your Code Here ----------------------- *)
(* basicType-> 'int'*)
(* Tok_Int *)

(* methodBody->(localDeclaration | statement)* *)
let rec parse_body l ret_list = 
  let (h,t) = lookahead l in
    match h with
      |Tok_Int -> let (local_return,toks) = parse_localDeclaration l in
        parse_body toks (ret_list@[local_return])
      |Tok_While -> let (statement_ret,toks) = parse_statement l in
        parse_body toks (ret_list@[statement_ret])
      |Tok_If -> let (statement_ret,toks) = parse_statement l in
        parse_body toks (ret_list@[statement_ret])
      |Tok_Id (name) -> let (assign_ret,toks) = parse_assign l in
        parse_body toks (ret_list@[assign_ret])
      |Tok_Print -> let (print_return,toks) = parse_print l in
        parse_body toks (ret_list@[print_return])
      |_ -> (ret_list,l)

(* localDeclaration->basicType ID ';' *)
and parse_localDeclaration l = 
  let (h,t) = lookahead l in 
    match h with Tok_Int -> let (h1,t1) = lookahead t in
    match h1 with Tok_Id (name) -> let (h2,t2) = lookahead t1 in
    match h2 with Tok_Semi -> (Define (Type_Int,(Id name)),t2)

(* statement->
    whileStatement
    |ifStatement
    |assignStatement
    |printStatement
*)

and parse_statement l = 
  let (h,t) = lookahead l in
    match h with
      |Tok_While -> let (while_ret, toks) = parse_while l in 
        (while_ret, toks) 
      |Tok_If -> let (if_ret, toks) = parse_if l in 
        (if_ret, toks) 
      |Tok_Id (name) -> let (assign_ret, toks) = parse_assign l in  
        (assign_ret, toks) 
      |Tok_Print -> let (print_ret,toks) = parse_print l in 
        (print_ret, toks) 

(* assignStatement->ID '=' exp ';' *)

and parse_assign l = 
  let (h,t) = lookahead l in
    match h with Tok_Id (name) -> let (h1,t1) = lookahead t in
    match h1 with Tok_Assign -> let (exp_ret,toks) = parse_exp t1 in 
    let (h2,t2) = lookahead toks in
    match h2 with Tok_Semi -> (Assign (Id name,exp_ret), t2)

and if_while l ret_list =  
  let (h,t) = lookahead l in  
    match h with  
    |Tok_If -> let (statement_ret, tok_list) = parse_statement l in 
      if_while tok_list (ret_list@[statement_ret]) 
    |Tok_While -> let (statement_ret, tok_list) = parse_statement l in 
      if_while tok_list (ret_list@[statement_ret])
    |Tok_Print -> let (statement_ret, tok_list) = parse_statement l in
      if_while tok_list (ret_list@[statement_ret])
    |Tok_Id (name) -> let (statement_ret,tok_list) = parse_statement l in 
      if_while tok_list (ret_list@[statement_ret])
    |Tok_RBrace -> (ret_list, l) 


(* ifStatement -> 'if' '(' exp ')'  '{' ( statement)* '}'  ( 'else' '{'( statement)* '}')? *)
and parse_if l = 
  let (h,t) = lookahead l in
  match h with Tok_If -> let (h1,t1) = lookahead t in
  match h1 with Tok_LParen -> let (exp_ret,toks) = parse_exp t1 in
  let (h2,t2) = lookahead toks in 
  match h2 with Tok_RParen -> let (h3,t3) = lookahead t2 in
  match h3 with Tok_LBrace -> let (h_ret,moreToks) = if_while t3 [] in
  let (h4,t4) = lookahead moreToks in
  match h4 with Tok_RBrace -> let (h5,t5) = lookahead t4 in 
  match h5 with 
    |Tok_Else -> (let (h6,t6) = lookahead t5 in
      match h6 with Tok_LBrace -> let (e_ret,toks3) = if_while t6 [] in
      let (h7,t7) = lookahead toks3 in
      match h7 with Tok_RBrace -> (If (exp_ret,List h_ret, List e_ret),t7))
    |_ -> (If (exp_ret, List h_ret,List []),t4)

(* whileStatement -> 'while''(' exp ')' '{'(statement )*'}' *)
and parse_while l = 
  let (h,t) = lookahead l in
  match h with Tok_While -> let (h2,t2) = lookahead t in
  match h2 with Tok_LParen -> let (exp_ret,toks) = parse_exp t2 in
  let (h3,t3) = lookahead toks in
  match h3 with Tok_RParen -> let (h4,t4) = lookahead t3 in
  match h4 with Tok_LBrace -> let (help_ret,tokens) = if_while t4 [] in
  let (h5,t5) = lookahead tokens in
  match h5 with Tok_RBrace -> (While (exp_ret,List help_ret),t5)

(* printStatement->'printf' '(' exp ')' ';' *)

and parse_print l = 
  let (h,t) = lookahead l in
    match h with Tok_Print -> let (h1,t1) = lookahead t in
    match h1 with Tok_LParen -> let (exp_ret,toks) = parse_exp t1 in
    let (h2,t2) = lookahead toks in 
    match h2 with Tok_RParen -> let (h3,t3) = lookahead t2 in
    match h3 with Tok_Semi -> (Print (exp_ret),t3)


(* exp -> additiveExp (('>'  | '<'  | '==' ) additiveExp *)
and parse_exp l = 
  let (addRet,toks) = parse_add l in
  let (h,t) = lookahead toks in
  match h with 
    |Tok_Greater -> let (expRet,ts) = parse_exp t in
      (Greater (addRet,expRet),ts)
    |Tok_Less -> let (expRet,ts) = parse_exp t in
      (Less (addRet,expRet),ts)
    |Tok_Equal -> let (expRet,ts) = parse_exp t in
      (Equal (addRet,expRet),ts)
    |_ -> (addRet,toks)

(* additiveExp -> multiplicativeExp ('+' multiplicativeExp)* *)
and parse_add l = 
  let (multRet,toks) = parse_multiply l in 
  let (h,t) = lookahead toks in 
  match h with 
    
    |Tok_Sum -> let (addRet,ts) = parse_add t in
      (Sum (multRet,addRet),ts) 
    |_ -> (multRet,toks)

(* multiplicativeExp-> powerExp ( '*' powerExp  )* *)
and parse_multiply l = 
  let (powerRet,toks) = parse_power l in
  let (h,t) = lookahead toks in
  match h with
    |Tok_Mult -> let (multRet,ts) = parse_multiply t in
      (Mult (powerRet,multRet),ts)
    |_ -> (powerRet,toks)

(* powerExp->primaryExp ( '^' primaryExp) * *)
and parse_power l = 
  let (primaryRet,toks) = parse_primaryExp l in
  let (h,t) = lookahead toks in
  match h with
    |Tok_Pow -> let (powerRet,ts) = parse_power t in
      (Pow (primaryRet,powerRet),ts)
    |_ -> (primaryRet,toks)

(* primaryExp->'(' exp ')' | ID | INITLIT *) 
and parse_primaryExp l = 
  let (h,t) = lookahead l in
    match h with 
    |Tok_LParen -> (let (exp_ret,toks) = parse_exp t in 
      let (h1,t1) = lookahead toks in
      match h1 with Tok_RParen -> (Paren (exp_ret),t1))
    |Tok_Id (name) -> (Id name, t)
    |Tok_Num (v) -> (Num v, t) 
    |_ -> raise (IllegalExpression "parse_primary") 
(* ID->( 'a'..'z' | 'A'..'Z') ( 'a'..'z' | 'A'..'Z' | '0'..'9')* *) 
(* INTLIT-> ('0' | ('1'..'9') ('0'..'9')* ) *)
(*  WS-> (' '|'\r'|'\t'|'\n') *)




let rec parse_Function lst = 
  (* mainMethod-> basicType 'main' '(' ')' '{' methodBody '}' *)
    let (h,t) = lookahead lst in 
      match h with Tok_Int -> let (h1,t1) = lookahead t in 
      match h1 with Tok_Main -> let (h2,t2) = lookahead t1 in
      match h2 with Tok_LParen -> let (h3,t3) = lookahead t2 in
      match h3 with Tok_RParen -> let (h4,t4) = lookahead t3 in
      match h4 with Tok_LBrace -> let (body_ret,toks) = parse_body t4 [] in 
      let (h5,t5) = lookahead toks in 
      match h5 with Tok_RBrace -> let (h6,t6) = lookahead t5 in
      match h6 with Tok_END ->
     (Fun (Type_Int,"main",List [], List body_ret),[h6]);;

(* ------------------------------------------------------*)





exception Error of int ;;




let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let tok_to_str t = ( match t with
          Tok_Num v -> string_of_int v
        | Tok_Sum -> "+"
        | Tok_Mult ->  "*"
        | Tok_LParen -> "("
        | Tok_RParen -> ")"
		| Tok_Pow->"^"
        | Tok_END -> "END"
        | Tok_Id id->id
		| Tok_String s->s
		| Tok_Assign->"="
		 | Tok_Greater->">"
		 | Tok_Less->"<"
		 | Tok_Equal->"=="
		 | Tok_Semi->";"
		 | Tok_Main->"main"
		 | Tok_LBrace->"{"
		 | Tok_RBrace->"}"
		 | Tok_Int->"int" 
		 | Tok_Float->"float"
		 | Tok_Print->"printf"
		 | Tok_If->"if"
		 | Tok_Else->"else"
		 | Tok_While-> "while"
    )

let print_token_list tokens =
	print_string "Input token list = " ;
	List.iter (fun x -> print_string (" " ^ (tok_to_str x))) tokens;
	print_endline ""
;;
	




(* -------------- Your Code Here ----------------------- *)

let rec spaces x = match x with  
  |0 -> print_string "" 
  |_ -> print_string "_"; spaces (x-1)
;;
let rec pretty_print pos q= match q with
  |Id (x) -> print_string x
  |Num (x) -> print_int x
  |Define (_,_) -> printDefine q pos
  |Assign (_,_) -> printAssign q pos
  |List p -> printList q pos
  |Fun (_, _, _, _) -> printFun q pos

  |Sum (_, _) -> printSum q pos
  |Greater (_, _)-> printGreater q pos
  |Equal (_, _) -> printEqual q pos
  |Less (_,_) -> printLess q pos
  |Mult (_,_) -> printMult q pos
  |Pow (_,_) -> printPow q pos
  |Print (_) -> printPrint q pos
  |If (_,_,_) -> printIf q pos
  |While (_,_) -> printWhile q pos
  |Paren (_) -> printParen q pos



and printDefine v pos = match v with
  |Define (x,y) -> spaces pos; print_string "int "; pretty_print pos y; print_string ";\n"; 

and printAssign v pos = match v with
  |Assign (x,y) -> spaces pos; pretty_print pos x; print_string " = "; pretty_print pos y; 
    print_string ";\n";

and printList v pos = match v with
  List p -> (match p with  
    |[] -> print_string ""
    |h::t -> pretty_print pos h; pretty_print pos (List t));

and printFun v pos = match v with
  |Fun (return_type, fun_name, arg_list, state_list) -> print_string "int main(){\n"; 
    pretty_print (4+pos) state_list; print_string "} "; 

and printSum v pos = match v with
  |Sum (x, y) -> (pretty_print pos x); print_string " + "; (pretty_print pos y); 

and printGreater v pos = match v with
  |Greater (x, y)-> (pretty_print pos x); print_string " > "; (pretty_print pos y); 

and printEqual v pos = match v with
  |Equal (x, y) -> (pretty_print pos x); print_string " == "; (pretty_print pos y);

and printLess v pos = match v with
  |Less (x, y) -> (pretty_print pos x); print_string " < "; (pretty_print pos y) ; 

and printMult v pos = match v with
  |Mult (x, y) -> (pretty_print pos x);  print_string " * "; (pretty_print pos y);

and printPow v pos = match v with 
  |Pow (x, y) -> (pretty_print pos x); print_string " ^ "; (pretty_print pos y); 

and printPrint v pos = match v with
  |Print (x) -> spaces pos; print_string "printf("; (pretty_print pos x); print_string ");\n"; 

and printIf v pos = match v with
  |If (cond, if_branch, else_branc) -> spaces pos; print_string "if("; (pretty_print pos cond); 
    print_string "){\n"; (pretty_print (4+pos) if_branch);  
    (match else_branc with  
      |List (astList) -> match astList with  
      |[] -> spaces pos; print_string "}\n" 
      |_ -> spaces pos; print_string "}else{\n";  
      (pretty_print (4+pos) else_branc); spaces pos; print_string "}\n"); 

and printWhile v pos = match v with
  |While (cond, body) -> spaces pos; print_string "while("; (pretty_print pos cond); print_string "){\n"; 
    (pretty_print (4+pos) body); spaces pos; print_string "}\n"; 

and printParen v pos = match v with
  |Paren (x) -> print_string "("; (pretty_print pos x); print_string ")";;

(* ----------------------------------------------------- *)


(*
you can test your parser and pretty_print with following code 
*)

(*

let prg1 = read_lines "main.c";;
let code = List.fold_left (fun x y->x^y) "" prg1;;	
let t = tokenize code;;
let (a,b)=parse_Function t;;

*)