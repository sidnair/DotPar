(* Abstract syntax tree definitions *)

open Str;;
module StringMap = Map.Make(String);;

type symbol_table = { 
  mutable table : string StringMap.t;
  mutable parent: symbol_table option; 
  mutable children : symbol_table list;
  mutable pure : bool
} 

type unop = Neg | Not
type binop =
    Add | Sub | Mult | Div | Mod
  | Eq | Neq | Lt | Leq | Gt | Geq
  | And | Or

type expression =
    Assignment_expression of expression * expression
  | Declaration of var_type * expression
  | Declaration_expression of var_type * expression * expression
  | Array_literal of expression list
  | List_comprehension of expression * param list * expression list * 
    expression * symbol_table
        (* unary operators *)
  | Unop of unop * expression
        (* all binary operators *)
  | Binop of expression * binop * expression
        (* postfix *)
  | Function_call of expression * expression list
  | Array_access of expression * expression
        (* *)
  | Variable of string
        (* constants *)
  | Char_literal of char
  | Number_literal of float
  | String_literal of string
  | Boolean_literal of bool
  | Nil_literal
      (* *)
  | Anonymous_function of var_type * param list * statements * symbol_table
  | Function_expression of statement  (* hacky as hell, but whatever *)
      (* *)
  | Empty_expression

and basic_type =
    Void_type
  | Number_type
  | Char_type
  | Boolean_type

and var_type =
    Basic_type of basic_type
  | Array_type of var_type
  | Fixed_array_type of var_type * expression
  | Func_type of var_type * var_type list
  | Func_param_type of var_type * param list

and param = Param of var_type * expression

and selection_statement = {
    if_cond : expression;
    if_body : statements;
    if_sym_tabl : symbol_table;
    else_body : statements;
    else_sym_tabl : symbol_table;
    elif_conds : expression list;
    elif_bodies : statements list;
    elif_sym_tabl : symbol_table list;
  }

and statement =
    Expression of expression
  | Statements of statements (* compound statements *)
  | Selection of selection_statement
  | Iteration of expression * expression * expression * statements *
    symbol_table
  | Jump of expression
  | Function_definition of string * var_type * param list * statements *
    symbol_table

and statements = statement list

and import = Import of string

and imports = import list

;;

type program = Program of imports * statements;;

(***********************************************************************)
let make_symbol_table = 
  let s_table = {
    table = StringMap.empty;
    parent = None;
    children = [];
    pure = false;
  } in
  s_table 

(***********************************************************************)
(* reverse string out for the AST *)

(* let ind = "  ";; *)

let rec string_of_expression expression =
  match expression with
    Assignment_expression(rv, lv) ->
      (string_of_expression rv) ^ "=" ^ (string_of_expression lv)
  | Declaration(type_dec, expr) ->
      (string_of_type type_dec) ^ " " ^ (string_of_expression expr)
  | Declaration_expression(type_dec, rv, lv) ->
      (string_of_type type_dec) ^ " " ^ (string_of_expression rv) ^
      "=" ^ (string_of_expression lv)
  | Array_literal(exprs) ->
      "[" ^ (String.concat ", " (List.map string_of_expression exprs)) ^ "]"
  | List_comprehension(expr, params, exprs, if_cond, s) ->
      "[" ^ (string_of_expression expr) ^ " for " ^
      (String.concat ", " (List.map string_of_param params)) ^ " in " ^
      (String.concat ", " (List.map string_of_expression exprs)) ^
      (match if_cond with
        Empty_expression -> ""
      | _ -> " if " ^ (string_of_expression if_cond))
      ^ "]"
        (* unary operators *)
  | Unop(op,expr) -> (string_of_unop op) ^ (string_of_expression expr)
        (* all binary operators *)
  | Binop(expr1,op,expr2) ->
      (string_of_expression expr1) ^
      (string_of_binop op) ^
      (string_of_expression expr2)
        (* postfix *)
  | Function_call(expr, exprs) ->
      (string_of_expression expr) ^ "(" ^
      (String.concat ", " (List.map string_of_expression exprs)) ^ ")"
  | Array_access(expr, expr2) ->
      (string_of_expression expr) ^ "[" ^ (string_of_expression expr2) ^ "]"
        (* *)
  | Variable(str) -> str
        (* constants *)
  | Char_literal(c) -> "'" ^ (String.make 1 c) ^ "'"
  | Number_literal(n) -> (string_of_float n)
  | String_literal(s) -> "\"" ^ s ^ "\""
  | Boolean_literal(b) -> (if (b) then "true" else "false")
  | Nil_literal -> "nil" (* is this legal? *)
      (* *)
  | Anonymous_function(type_def, params, block, sym_tabl) ->
      "func:" ^ (string_of_type type_def) ^ "(" ^
      (String.concat ", " (List.map string_of_param params)) ^ ")" ^
      "{" ^
      (string_of_statements block) ^ "}"
  | Function_expression(state) ->
      (string_of_statement state)
      (* *)
  | Empty_expression -> ""

and string_of_unop op =
  match op with
    Neg -> "-"
  | Not -> "not"
and string_of_binop op =
  match op with
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

and string_of_basic_type btype =
  match btype with
    Void_type -> "void"
  | Number_type -> "number"
  | Char_type -> "char"
  | Boolean_type -> "bool"
and string_of_type var_type =
  match var_type with
    Basic_type(b) -> (string_of_basic_type b)
  | Array_type(a) -> (string_of_type a) ^ "[]"
  | Fixed_array_type(a,expr) ->
      (string_of_type a) ^ "[" ^ (string_of_expression expr) ^ "]"
  | Func_type(ret_type, param_types) ->
      "func:" ^ (string_of_type ret_type) ^ "(" ^
      (String.concat ", " (List.map string_of_type param_types)) ^ ")"
  | Func_param_type(ret_type, params) ->
      "func:" ^ (string_of_type ret_type) ^ "(" ^
      (String.concat ", " (List.map string_of_param params)) ^ ")"

and string_of_param parm =
  match parm with
    Param(param_type, varname) ->
      (string_of_type param_type) ^ " " ^ (string_of_expression varname)

and string_of_selection select =
  "if(" ^ (string_of_expression select.if_cond) ^ ")" ^
    "{" ^ (string_of_statements select.if_body) ^ "}" ^
      (if ((List.length select.elif_conds) != 0) then
        let gen_elif cond body =
          "elif(" ^ (string_of_expression cond) ^ ")" ^
          "{" ^ (string_of_statements body) ^ "}"
        in 
        (String.concat ""
           (List.map2 gen_elif select.elif_conds select.elif_bodies))
      else "") ^
      (if (select.else_body != []) then
        "else {" ^ (string_of_statements select.else_body) ^ "}"
      else "")

and string_of_statement stat =
  match stat with
    Expression(e) -> (string_of_expression e) ^ ";\n"
  | Statements(s) -> (string_of_statements s) ^ "\n"
  | Selection(s) -> (string_of_selection s) ^ "\n"
  | Iteration(dec,check,incr, stats, sym_tabl) ->
      "for(" ^ (string_of_expression dec) ^ "," ^
      (string_of_expression check) ^ "," ^
      (string_of_expression incr) ^ ")" ^
      "{" ^ (string_of_statements stats) ^ "}\n"
  | Jump(j) -> "return " ^ (string_of_expression j) ^ ";\n"
  | Function_definition(name, ret_type, params, sts, sym_tabl) ->
      "func " ^ name ^ ":" ^ (string_of_type ret_type) ^
      "(" ^ (String.concat ", " (List.map string_of_param params)) ^ ")" ^
      "{\n" ^ (string_of_statements sts) ^ "}\n"
and string_of_statements statements =
  match statements with
    head::tail -> (string_of_statement head) ^"\n"^ (string_of_statements tail)
  | _-> ""

and string_of_import import =
  match import with
    Import(s) -> "import " ^ s ^ ";\n"
and string_of_imports imports =
  match imports with
    head::tail -> (string_of_import head) ^ (string_of_imports tail)
  | _-> ""
;;
let string_of_program program =
  match program with
    Program(imp, stat) ->
      (string_of_imports imp) ^ "\n" ^ (string_of_statements stat)
;;
