(* Check Semantics of the AST *)
(* Ripped off rom Generator *)

open Ast
open Str

(*define exceptions *)
exception Symbol_undefined of string
exception Func_id_not_found of string
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

let rec lookup_in table id iteration =
  try
   (* Make sure this is returning the find !!! *) 
    StringMap.find id table.table
      (* iteration = iteration + 1 *)
  with Not_found ->
    match table.parent with
      Symbol_table(p) -> lookup_in p id (iteration + 1)
    | Nil    -> raise (Not_found)

let add_to_symbol_table id id_type sym_tabl = 
  StringMap.add id_type id sym_tabl.table
  (* !!! Check thistuff to given sym_tabl*)

let make_symbol_table sym_tabl = 
  let init_table = List.fold_left (fun tableMaker (id, element) -> 
    StringMap.add id element tableMaker)
    StringMap.empty
    []
  in
  let symbol_table = { 
    table = init_table;
    parent = sym_tabl;
    property = false;}
  in
  symbol_table

(***********************************************************************)

let rec check_expression expression sym_tabl =
  (fun check_expression_table (expr) -> 
    check_expression expr sym_tabl)
  (fun check_param_table (param) ->
    check_param param sym_tabl)
  (match expression with
  | Assignment_expression(left, right) ->
      (try
        (let rec get_left_side expr =
          (match expr with
            Variable(v) ->
              let var = lookup_in sym_tabl v 0 in
              compare_type var (check_expression right sym_tabl);
              var (* return new symbol table? *)
          | Array_access(name, index) ->
              (get_left_side name)
                (* compare_type var (check_expression right sym_tabl) *)
          | _ -> raise (Error "FUCK YOUUUUUUUUUUU")
          )
        in
        (get_left_side left)
        )
      with Not_found -> raise (Error "Variable undefined")
      )
  | Declaration(type_dec, var) ->
      (match var with
        Variable(v) ->
          (try
            ignore (lookup_in sym_tabl v 0);
            sym_tabl.table (* hack to work atm *)
          with Not_found ->
            (* raise (Error "aoeu") *)
            add_to_symbol_table v (check_var_type type_dec) sym_tabl
          )
      | _ -> raise (Error "SHITS FUCKED YO")
      );
      raise (Error "Variable previous defined")
  | Declaration_expression(type_dec, left, right) ->
      try
        (match left with
          Variable(v) ->
            ignore(lookup_in sym_tabl v 0);
        | _ -> raise (Error "SHITS FUCKED YO")
        );
        raise (Error "Variable previous defined") 
      with Not_found ->
        compare_type type_dec (check_expression right sym_tabl);
        add_to_symbol_table var (check_var_type type_dec) sym_tabl
  | Array_literal(exprs) ->
    let check_expression_table expr = 
      check_expression expr sym_tabl
    in
    (List.fold_left compare_type exprs.hd  
      (List.map check_expression_table exprs))
  | List_comprehension(expr, params, exprs, if_cond) ->
    let symbol_table = make_symbol_table sym_tabl in
    (check_expression expr symbol_table)
    (List.map check_param_table params)
    (List.fold_left compare_type exprs.hd  
      (List.map check_expression_table exprs))
    (match if_cond with
        Empty_expression -> ""
      | _ -> (check_expression if_cond symbol_table))
  | Unop(op,expr) ->
      check_unop op (check_expression expr sym_tabl) sym_tabl 
  | Binop(expr1,op,expr2) -> 
      (check_operator 
        (check_expression expr1 sym_tabl)
        op
        (check_expression expr2 sym_tabl))
  | Function_call(expr, exprs) ->
      (check_expression expr sym_tabl)
      List.map check_expression_table exprs
  | Array_access(expr, expr2) ->
         (* Check to make sure expr2 is inbounds with expr *)
      (check_expression expr sym_tabl)
      (check_expression expr2 sym_tabl)
  | Variable(str) -> str
  | Char_literal(c) -> "Char"
  | Number_literal(n) -> "Number"
  | String_literal(s) -> "String"
  | Boolean_literal(b) -> "Boolean"
  | Nil_literal -> "nil"
  | Anonymous_function(type_def, params, block) ->
      (check_var_type type_def sym_tabl)
      (List.map check_param_table params)
      (check_statements block sym_tabl)
  | Function_expression(state) ->
      (match state with
      Function_definition(name, ret_type, params, sts) ->
          (check_func_def name ret_type params sts sym_tabl) 
      | _ -> (Error "Illegal statement") )
  | Empty_expression -> ""
  | _ -> raise (Error "Expression type not valid"))

and check_unop op type1 sym_tabl =
  match op with
    Neg -> if (type1 <> "Number")
            then raise (Error "Operator applied invalid type")
  | Not -> if (type1 <> "Boolean")
            then raise (Error "Operator applied to invalid type")
  | _ -> rasie ("Unsupported Unary Operator")

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
  | Func_type(ret_type, param_types) ->
      (check_var_type ret_type)
      (List.map check_var_type param_types)
 | Func_param_type(ret_type, params) ->
    let extract_type param = 
      match param with
        Param(param_type, varname) -> param_type
    in
    let type_list = (List.map extract_type params) in
    (check_var_type ret_type)
    (List.map check_param params)
  | _ -> raise (Error "Unsupported variable type")

and check_param parm sym_tabl =
  match parm with
    Param(param_type, varname) ->
      (add_to_symbol_table
        (check_expression varname sym_tabl)
        (check_var_type param_type sym_tabl)
        sym_tabl)

and check_cond cond = 
  match cond with
    Boolean_type -> true
  | _ -> raise Error ("Conditional not valid")

and check_selection select sym_tabl = 
  (check_expression select.if_cond sym_tabl)
  (check_cond select.if_cond) 
  (check_statements select.if_body (make_symbol_table sym_tabl))
  (if ((List.length select.elif_conds) != 0) then
    let check_elif cond body  = 
      (check_cond cond) 
      (check_statements body (make_symbol_table sym_tabl))
    in
    (List.map2 check_elif select.elif_conds select.elif_bodies)
  else "") 
  (if(select.else_body != []) then 
      (check_statements select.else_body (make_symbol_table sym_tabl))
  else "")

and check_func_definition name ret_type params stats sym_tabl = 
  try
    ignore (lookup_in sym_tabl name 0)
    raise Error("More than one function found with same identifier")
  with Not_found ->
    (* Double check this is what we want to do with func_types *)
    (add_to_symbol_table name (check_var_type ret_type) sym_tabl)
    List.map check_param params symbol_table
    check_statements stats symbol_table

and check_iter dec check incr stats parent_sym_tabl =
  let symbol_table = make_symbol_table parent_sym_tabl in
  (check_expression dec symbol_table)
  (check_expression check symbol_table)
  (check_statements stats symbol_table)
  (check_expression incr symbol_table)
    (* !!! Iteration stuff + symbol_table *)
(* Check to make sure check is bool *)

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
  let symbol_table = make_symbol_table Nil in
  let sast = check_statements stat symbol_table in
    sast
;;
