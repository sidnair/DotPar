(* translate into scala *)
open Ast
open Str

(*define exceptions *)
exception Symbol_undefined of string
exception Func_id_not_found of string

exception Unequal_number_args
exception Unequal_type_args

exception Func_side_effects
exception Error of string

(* Execution point *)
let translate ast =
  let table = List.fold_left (fun tableMaker (id, element) -> StringMap.add id element tableMaker) StringMap.empty in
  let global_sym_table = { init = table; parent = None }

(* lookup in string table *)
let rec lookup_table table id =
  try
    StringMap.find id table.table 
  with Not_found  ->
    match table.parent with
      Some(p) -> lookup_table p id
    | None    -> raise (Symbol_undefined id)

(* Build string table *)

(***********************************************************************)
(* reverse string out for the AST *)

let rec check_expression expression =
  match expression with
    Assignment_expression(rv, lv) ->
        if( (check_expression rv) <> (check_expression lv) )
            raise ("Assignment operation invalid, types do not match")
  | Declaration(type_dec, expr) ->
      (* Add something to symbol table here, aka wtf !!!*)     
      (check_type type_dec) ^ " " ^ (check_expression expr)
  | Declaration_expression(type_dec, rv, lv) ->
      (* Add something to symbol table here, aka wtf !!!*)     
      (check_type type_dec) ^ " " ^ (check_expression expr)
      (check_type type_dec) ^ " " ^ (check_expression rv) ^
      "=" ^ (check_expression rv)
  | Array_literal(exprs) ->
     (* Go through map, check to make sure all types match !!! *) 
        (List.map check_expression exprs)
  | List_comprehension(expr, params, exprs, if_cond) ->
      (check_expression expr)
      (List.map check_param params)
      (List.map check_expression exprs)
      (match if_cond with
        Empty_expression -> ""
      | _ -> (check_expression if_cond))
        (* unary operators *)
  | Unop(op,expr) -> 
          (check_unop op)
          (check_expression expr)
        (* all binary operators *)
  | Binop(expr1,op,expr2) ->
      (check_expression expr1)
      (check_binop op)
      (check_expression expr2)
        (* postfix *)
  | Function_call(expr, exprs) ->
      (check_expression expr)
      (List.map check_expression exprs)
  | Array_access(expr, expr2) ->
         (* Check to make sure expr2 is inbounds with expr *) 
      (check_expression expr)
      (check_expression expr2)
        (* *)
  | Variable(str) -> str
        (* constants *)
  | Char_literal(c) -> "Char_literal"
  | Number_literal(n) -> "Number_literl"
  | String_literal(s) -> "String_literal"
  | Boolean_literal(b) -> "Boolean_literal"
  | Nil_literal -> "nil"
      (* *)
  | Anonymous_function(type_def, params, block) ->
      (check_type type_def)
      (List.map check_param params)
      (check_statements block)
  | Function_expression(state) ->
      (check_statement state)
      (* *)
  | Empty_expression -> ""
  | _ -> raise ("Expression type not valid") 

and check_unop op =
  match op with
    Neg -> "-"
  | Not -> "not"
  | _ -> rasie ("Unsupported Unary Operator")

and check_binop op =
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
  | _ -> raise ("Unsupported Binary Operator")   

and check_basic_type btype =
  match btype with
    Void_type -> "void"
  | Number_type -> "number"
  | Char_type -> "char"
  | Boolean_type -> "bool"
  | _ -> raise ("Unsupported Type")
and check_type var_type =
  match var_type with
    Basic_type(b) -> (check_basic_type b)
  | Array_type(a) -> (check_type a)
  | Fixed_array_type(a,expr) ->
      (check_type a)
      (check_expression expr)
  | Func_type(ret_type, param_types) ->
      (check_type ret_type)
      (List.map check_type param_types)
  | Func_param_type(ret_type, params) ->
      (check_type ret_type)
      (List.map check_param params)
  | _ -> raise ("Unsupported variable type")    

and check_param parm =
  match parm with
    Param(param_type, varname) ->
      (check_type param_type)
      (check_expression varname)

and is_bool var =
    match var with
        Boolean_type -> "" 
        | _ -> raise ("Conditinal not of boolean type")

and check_selection select =
    is_bool select.if_cond 
    check_statements select.if_body
      (if ((List.length select.elif_conds) != 0) then
        let gen_elif cond body =
            is_bool cond
          check_statements body
        in 
           (List.map2 gen_elif select.elif_conds select.elif_bodies)
      (if (select.else_body != []) then
        check_statements select.else_body)

and check_statement stat =
  match stat with
    Expression(e) -> (check_expression e) 
  | Statements(s) -> (check_statements s) 
  | Selection(s) -> (check_selection s) 
  | Iteration(dec,check,incr, stats) ->
      (check_expression dec)
      (check_expression check)
      (check_expression incr)
      (check_statements stats)
  | Jump(j) -> 
          (check_expression j)
  | Function_definition(name, ret_type, params, sts) ->
      (check_type ret_type)
      (List.map check_param params)
      (check_statements sts)
and check_statements statements =
  match statements with
    head::tail -> 
        (check_statement head)  
        (check_statements tail)
  | _ -> ""

let check_program program =
  match program with
    Program(imp, stat) -> (check_statements stat)
;;
