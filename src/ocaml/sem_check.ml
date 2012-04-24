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
module StringMap = Map.Make(String);;

type symbol_table = {
    table : string StringMap.t;
    parent : symbol_table_ref;
  }
and symbol_table_ref = Symbol_table of symbol_table | Nil;;

let table = List.fold_left
    (fun tableMaker (id, element) -> StringMap.add id element tableMaker)
    StringMap.empty (* initial value *)
    [] (* builtins *);;
let global_sym_table = { table = table; parent = Nil; };;

let translate ast =
  ast
;;

(* lookup in string table *)
let rec lookup_table table id =
  try
    StringMap.find id table.table
  with Not_found ->
    match table.parent with
      Symbol_table(p) -> lookup_table p id
    | Nil    -> raise (Symbol_undefined id)

(* Build string table *)

(***********************************************************************)
(* reverse string out for the AST *)

let rec check_expression expression =
  match expression with
    Assignment_expression(rv, lv) ->
        if( (check_expression rv) <> (check_expression lv) ) then
            raise (Error "Assignment operation invalid, types do not match")
  | Declaration(type_dec, expr) ->
          if ( (check_expression expr) <> (check_type type_dec)) then
              raise (Error "Declaration operation invalid, types do not match")
  (*      if( (lookup_table table (check_expression expr)) 
            = Symbol_undefined ) then table.add expr *)
    else raise (Error "Variable already delcared within this scope") 
  | Declaration_expression(type_dec, lv, rv) ->
    (* if( lookup_table table (check_expression lv) == Symbol_undefined ) then 
            table.add expr;
    else raise (Error "Variable already delcared within this scope") *) 
    if ( not(compare_type type_dec (check_expression lv))) then
        raise (Error "Declaration is invalid")
  (*| Array_literal(exprs) -> *)
     (* Go through map, check to make sure all types match !!! *)
     (*   List.fold_left compare_type exprs.hd (List.map check_expression exprs)
*)  (* we don't know what to do !!! *)
  (*
  | List_comprehension(expr, params, exprs, if_cond) ->
      (check_expression expr)
      (List.map check_param params)
      (List.map check_expression exprs)
      (match if_cond with
        Empty_expression -> ""
      | _ -> (check_expression if_cond)) *)
        (* unary operators *)
  | Unop(op,expr) ->
          (check_unop op)
          (check_expression expr)
        (* all binary operators *)
        
  (* | Binop(expr1,op,expr2) -> *)
      (* match on operation, check type makes sense (+ number, && bool, etc) *)
  
    (* (compare_type (check_expression expr1) (check_expression expr2)) *)
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
  | _ -> raise (Error "Expression type not valid")

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
  | _ -> raise (Error "Unsupported Binary Operator")

(* just have to compare types *)
and compare_type type1 type2 =
    if( type1 <> type2) then raise (Error "Types are not equivalent")
    else true

and check_operator type1 op type2 =
    match op with
    Add -> if (type1 <> "Number") then raise (Error "Operator applied invalid type")
  | Sub -> if (type1 <> "Number") then raise (Error "Operator applied invalid type")
  | Mult -> if (type1 <> "Number") then raise (Error "Operator applied invalid type")
  | Div -> if (type1 <> "Number") then raise (Error "Operator applied invalid type")
  | Mod -> if (type1 <> "Number") then raise (Error "Operator applied invalid type")
  | Eq -> "" 
  | Neq -> ""
  | Lt -> if (type1 <> "Number") then raise (Error "Operator applied invalid type")
  | Leq -> if (type1 <> "Number") then raise (Error "Operator applied invalid type")
  | Gt -> if (type1 <> "Number") then raise (Error "Operator applied invalid type")
  | Geq -> if (type1 <> "Number") then raise (Error "Operator applied invalid type")
  | And -> if (type1 <> "Boolean") then raise (Error "Operator applied invalid type")
  | Or -> if (type1 <> "Boolean") then raise (Error "Operator applied invalid type")
  | _ -> raise (Error "Unsupported Binary Operator")


and check_basic_type btype =
  match btype with
    Void_type -> "void"
  | Number_type -> "number"
  | Char_type -> "char"
  | Boolean_type -> "bool"
  | _ -> raise (Error "Unsupported Type")

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
  | _ -> raise (Error "Unsupported variable type")

and check_param parm =
  match parm with
    Param(param_type, varname) ->
      (check_type param_type)
      (compare_type param_type (check_expression varname))

and is_bool var =
    match var with
        Boolean_type -> ""
        | _ -> raise (Error "Conditinal not of boolean type")

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
        check_statements select.else_body))

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
