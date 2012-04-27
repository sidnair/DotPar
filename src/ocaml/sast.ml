(* Check Semantics of the AST *)
(* Ripped off rom Generator *)

open Ast
open Str

(*define exceptions *)
exception Symbol_undefined of string
exception Func_id_not_found of string
exception Not_found of string
exception Unequal_number_args
exception Unequal_type_args

exception Func_side_effects
exception Error of string

module StringMap = Map.Make(String);;

type symbol_table = {
  table : string StringMap.t;
  parent : symbol_table_ref;
  property : bool;     
}

and symbol_table_ref = Symbol_table of symbol_table | Nil;;

(* return the symbol_number recrusive, and how things we had to recurse *)
(* lookup in string table *)
let rec lookup_in table id iteration =
  try
    StringMap.find id table.table
    iteration = iteration + 1
  with Not_found ->
    match table.parent with
      Symbol_table(p) -> lookup_table p id
    | Nil    -> raise (Not_found id)

let make_symbol_tabl sym_tabl = 
  let init_table = List.fold_left (fun tableMaker (id, element) -> 
    StringMap.add id element tableMaker)
    StringMap.empty
    [] 
    (* Utility to make an empty symbol tabl *)
(***********************************************************************)

let rec check_expression expression =
  match expression with
    Assignment_expression(rv, lv) ->
      (* if( (get_type rv) <> (get_type lv) ) then *)
        (* doop doop doop *)
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
      (* check if it's a function definition *)
      (match state with
      Function_definition(name, ret_type, params, sts) ->
          (check_func_def name ret_type params sts sym_tabl) 
      | _ -> (Error "Illegal statement") )
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
    Add ->
      if (type1 <> "Number" or type2 <> "Number")
      then raise (Error "Operator applied invalid type")
  | Sub ->
      if (type1 <> "Number" or type2 <> "Number")
      then raise (Error "Operator applied invalid type")
  | Mult ->
      if (type1 <> "Number" or type2 <> "Number")
      then raise (Error "Operator applied invalid type")
  | Div ->
      if (type1 <> "Number" or type2 <> "Number")
      then raise (Error "Operator applied invalid type")
  | Mod ->
      if (type1 <> "Number" or type2 <> "Number")
      then raise (Error "Operator applied invalid type")
  | Eq ->
      if (type1 <> type2)
      then raise (Error "Types do not match on both sides of the operator")
  | Neq ->
      if (type1 <> type2)
      then raise (Error "Types do not match on both sides of the operator")
  | Lt ->
      if (type1 <> "Number" or type2 <> "Number")
      then raise (Error "Operator applied invalid type")
  | Leq ->
      if (type1 <> "Number" or type2 <> "Number")
      then raise (Error "Operator applied invalid type")
  | Gt ->
      if (type1 <> "Number" or type2 <> "Number")
      then raise (Error "Operator applied invalid type")
  | Geq ->
      if (type1 <> "Number" or type2 <> "Number")
      then raise (Error "Operator applied invalid type")
  | And ->
      if (type1 <> "Boolean" or type2 <> "Boolean")
      then raise (Error "Operator applied invalid type")
  | Or ->
      if (type1 <> "Boolean" or type2 <> "Boolean")
      then raise (Error "Operator applied invalid type")
  | _ -> raise (Error "Unsupported Binary Operator")


and check_basic_type btype =
  match btype with
    Void_type -> "Void"
  | Number_type -> "Number"
  | Char_type -> "Char"
  | Boolean_type -> "Boolean"
  | _ -> raise (Error "Unsupported Type")

and check_var_type var_type =
  match var_type with
    Basic_type(b) -> (check_basic_type b)
  | Array_type(a) -> (check_basic_type a)  ^ "[]"
  (*| Func_type(ret_type, param_types) ->*)
      (*(check_var_type ret_type)*)
      (*(List.map check_var_typeype param_types)*)
  | Func_type(ret_type, params) -> 
      List.map check_var_type param_types
      check_var_type ret_type
  | Func_param_type(ret_type, params) ->
      let extract_type param = 
        match param with
        | Param(param_type, varname) -> param_type
      in
      let type_list = (List.map extract type params) 
      (check_var_type ret_type)
      (List.map check_param params)
  | _ -> raise (Error "Unsupported variable type")

and check_param parm sym_tabl=
  match parm with
    Param(param_type, varname) ->
      try
        ignore (look_up sym_tabl name 0);
        raise Error("More than one function found with same identifier")
      with Not_found _ ->
        ""    
    (*let build_table =  *)
      (*[> !!! BUILD TABLE ^<]*)

and check_selection select sym_tabl =
  let check_if_bool var =
    check_expression car sym_tabl
    match var with
      Boolean_type -> true
    | _ -> raise Error("Conditonal not a boolean")
  in 
  (check_if_bool select.if_cond) 
  (check_expression select.if_cond sym_tabl)
  (check_statements select.if_body sym_tabl)
  (if ((List.length select.elif_conds) != 0) then
    let check_elif cond body sym_tabl = 
      (check_if_bool cond) 
      (check_statements body sym_tabl)
    in
    (List.map2 check_elif select.elif_conds select.elif_bodies sym_tabl)
  else "") 
  (if(select.else_body != []) then 
      (check_statements select.else_body sym_tabl)i
  else "")

and check_func_def name ret_type params stats sym_tabl = 
  try
    ignore (look_up sym_tabl name 0);
    raise Error("More than one function found with same identifier")
  with Not_found _ ->
    (*let build_table =  *)
    (*[> !!! Build table <] *)
    (*in*)
      (check_type ret_type)
    (List.map check_param params)
    (check_statements sts)

and check_iter dec check incr stats parent_sym_tabl =
  let symbol_table = { table = init_table;
                       parent = parent_sym_tabl;
                       property = false;}
  check_expression dec symbol_table
  check_expression check symbol_table
  check_expression stats symbol_table
  check_expression incr symbol_table

and check_statement stat sym_tabl =
  match stat with
    Expression(e) -> (check_expression e sym_tabl)
  | Statements(s) -> (check_statements s sym_tabl)
  | Selection(s) -> (check_selection s sym_tabl)
  | Iteration(dec,check,incr, stats) -> 
      (check_iter dec check incr stats sym_tabl)
  | Jump(j) -> (check_expression j)
  | Function_definition(name, ret_type, params, sts) ->
      (check_func_def name ret_type params sts sym_tabl)

and check_statements statements sym_tabl =
  match statements with
    head::tail ->
      (check_statement head sym_tabl)
      (check_statements tail sym_tabl)
  | _ -> raise Error("Malformed statement")

let generate_sast program =
  match program with
    Program(imp, stat) -> 
   let symbol_table = { table    = init_table; 
                         parent   = Nil; 
                         property = false}
    let sast = check_statements stat symbol_table
    sast
;;
