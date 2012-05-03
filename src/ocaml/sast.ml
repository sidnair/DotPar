(* Check Semantics of the AST *)
(* Ripped off rom Generator *)

open Ast
open Str




(* define exceptions *)
exception Symbol_undefined of string
exception Func_id_not_found of string
exception Unequal_number_args
exception Unequal_type_args

exception Func_side_effects
exception Error of string

module StringMap = Map.Make(String);;

type symbol_table = {
  table : string StringMap.t;
  parent : symbol_table;
  property : bool;     
  children : symbol_table list;
}

let global_table = ref None;;

let ht = Hashtbl.create 100;;

let rec lookup_in table id iteration : string =
  try
   (* Returns the variables type !!! *) 
    StringMap.find id table.table
  with Not_found ->
    match table.parent with
    symbol_table -> lookup_in symbol_table id (iteration + 1) 
    | _ -> raise (Not_found) 

(*let rec get_symbol_table table id iteration = *)
  (* This uses a wrapper method, get_sym table to map get_sym_table for each
   * child of a symbol_table *)
  (*let get_sym table =*)
    (*(match table with*)
    (*| symbol_table -> get_symbol_table table id (iteration + 1)*)
    (*| _ -> raise (Not_found));*)
  (*in*)
  (*(try *)
    (*ignore (StringMap.find id table.table);  *)
    (*table *)
  (*with Not_found ->*)
    (*ignore(List.iter get_sym table.children); *)
    (*table*)
  (*)*)

let add_to_symbol_table id id_type sym_tabl = 
  sym_tabl.table = StringMap.add id_type id sym_tabl.table

let make_symbol_table sym_tabl = 
   (*let init_table = *)
    (*List.fold_left (fun tableMaker (id, element) -> *)
    (*StringMap.add id element tableMaker)*)
    (*StringMap.empty*)
    (*[]*)
  (*in*)
  let symbol_table = { 
    table = StringMap.empty;
    parent = sym_tabl;
    property = false;
    children = [];} 
  in 
  symbol_table :: sym_tabl.children

(***********************************************************************)

let rec check_expression expression sym_tabl =
  let check_expression_table expr = get_type expr sym_tabl in
  let check_param_table param =  
    check_param param sym_tabl in
  (match expression with
  | Assignment_expression(left, right) ->
      (try
        (let rec get_left_side expr =
          (match expr with
            Variable(v) ->
              let var = lookup_in sym_tabl v 0 in
              (*compare_type var (get_type right sym_tabl);*)
              ignore(compare_type "" "");
              (*var [> return new symbol table? <]*)
              ""
          | Array_access(name, index) ->
              (get_left_side name)
                (* compare_type var (check_expression right sym_tabl) *)
          ""
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
          with Not_found ->
             ignore(add_to_symbol_table v (check_var_type type_dec) sym_tabl);
        )
      | _ -> raise (Error "SHITS FUCKED YO")
      );
      raise (Error "Variable previous defined")
  | Declaration_expression(type_dec, left, right) ->
    (match left with
      Variable(v) ->  
        (try 
          ignore (lookup_in sym_tabl v 0);
          (*sym_tabl.table*)
        with Not_found ->
        (*(compare_type *)
          (*(check_var_type type_dec) *)
          (*(check_expression right sym_tabl))*)
          ignore(compare_type "" "" );
          ignore(add_to_symbol_table v (check_var_type type_dec) sym_tabl))
    | _ -> raise (Error "ERROR")
    );
    raise (Error "Variable perviously defined") 
  | Array_literal(exprs) ->
     (try
        let get_literal_type expr =
          (match expr with
          | Variable(v) -> (lookup_in sym_tabl v 0) 
          | Char_literal(b) -> "Char" 
          | Number_literal(b) -> "Number" 
          | String_literal(b) -> "String"
          | Boolean_literal(b) -> "Boolean" 
          | _ -> raise (Error "Type not understood"));
        in
       (* DOESN'T ACTUALLY DO ANYTHING *)(* DOESN'T ACTUALLY DO ANYTHING *)(*
       DOESN'T ACTUALLY DO ANYTHING *)(* DOESN'T ACTUALLY DO ANYTHING *)(*
       DOESN'T ACTUALLY DO ANYTHING *)(* DOESN'T ACTUALLY DO ANYTHING *)(*
       DOESN'T ACTUALLY DO ANYTHING *)(* DOESN'T ACTUALLY DO ANYTHING *)(*
       DOESN'T ACTUALLY DO ANYTHING *) 
        (*ignore (List.map2 compare_type *)
            (*(List.map get_literal_type exprs)*)
          (*(get_literal_type (List.hd exprs))  *)
            (*(List.map get_literal_type exprs)*)
          (*);*)
          ""
      with Not_found -> raise (Error "Variable undefined")
      )
 (*| List_comprehension(expr, params, exprs, if_cond) ->*)
     (*let symbol_table = make_symbol_table sym_tabl in*)
     (*(check_expression expr symbol_table);*)
     (*(List.map check_param_table params);*)
    (*[>(List.fold_left compare_type (List.hd exprs) <]*)
      (*[>(List.map check_expression_table exprs))<]*)
    (*(match if_cond with*)
        (*Empty_expression -> ""*)
      (*| _ -> (check_expression if_cond symbol_table))*)
  | Unop(op,expr) ->
      (check_unop op (get_type expr sym_tabl) sym_tabl) 
  | Binop(expr1,op,expr2) -> 
      (check_operator 
        (get_type expr1 sym_tabl)
        op
        (get_type expr2 sym_tabl))
  | Function_call(expr, exprs) ->
      (* match expr with variable, and if found, retrieve param list
       * from when function was defined by global hash
       * and check params to the function all *)
      let get_func_name expr =
        let v = (check_expression expr sym_tabl) in 
     (try 
        ignore (lookup_in sym_tabl v 0);
        (*ignore (List.map2 compare_type *)
                  (*(Hashtbl.find ht v)*)
                  (*(List.map check_expression_table exprs));*)
    "" (* !!! This doesn't work actually... *) 
    with Not_found -> raise (Error "Function used but not declared")
     )
  in
  (get_func_name expr)
  | Array_access(expr, expr2) ->
     (try
       (match expr with
        | Variable(v) -> ignore (lookup_in sym_tabl v 0);
        ""
        | _ -> raise (Error "Malformed array access call")
      )
      with Not_found -> raise (Error "Array used while not defined")
     )
  | Variable(str) -> str
  | Char_literal(c) -> "Char"
  | Number_literal(n) -> "Number"
  | String_literal(s) -> "String"
  | Boolean_literal(b) -> "Boolean"
  | Nil_literal -> "nil"
  | Anonymous_function(type_def, params, block) ->
      (check_var_type type_def);
      (List.map check_param_table params);
      (check_statements block sym_tabl);
  | Function_expression(state) ->
      (match state with
      Function_definition(name, ret_type, params, sts) ->
          (check_func_definition name ret_type params sts sym_tabl) 
      | _ -> raise (Error "Illegal statement") )
  | Empty_expression -> ""
  | _ -> raise (Error "Expression type not valid")
  )

and get_type expression sym_tabl :string =
  (match expression with 
  | Assignment_expression (expr, expr1) -> ""
  | Declaration (var_type, expr) -> ""
  | Declaration_expression (var_type, expr, expr1) -> ""
  | Array_literal (exprs) -> ""
  | List_comprehension (expr, params, exprs, expr1) -> ""
  | Unop (op, expr) -> ""
  | Binop (expr, op, expr1) -> ""
  | Function_call (expr, exprs) -> ""
  | Array_access (expr, expr1) -> ""
  | Variable (v) -> ""
  | Char_literal (c) -> ""
  | Number_literal (f) -> ""
  | String_literal (str) -> ""
  | Boolean_literal (b) -> ""
  | Nil_literal -> ""
  | Anonymous_function (var_type, params, stats) -> ""
  | Function_expression (stat) -> ""
  | Empty_expression -> ""
  )
and check_unop op type1 sym_tabl =
  match op with
    Neg -> if (type1 <> "Number") 
            then raise (Error "Operator applied invalid type") else ""
  | Not -> if (type1 <> "Boolean")
            then raise (Error "Operator applied to invalid type") else ""

(* just have to compare types *)
and compare_type type1 type2 =
  (*if( type1 <> type2) then raise (Error "Types are not equivalent")*)
  (*else "Fuck" *)
  ""

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
      sym_tabl.table = (add_to_symbol_table
        (check_expression varname sym_tabl)
        (check_var_type param_type sym_tabl)
        sym_tabl)
    sym_tabl

and check_cond cond = 
  match cond with
    Boolean_type -> true
  | _ -> raise (Error "Conditional not valid")

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
    raise (Error "More than one function found with same identifier")
  with Not_found ->
    (* Double check this is what we want to do with func_types *)
    symbol_table.table = (add_to_symbol_table name (check_var_type ret_type) sym_tabl)
    (Hashtbl.add ht name (List.map check_param params symbol_table))
    (check_statements stats symbol_table);
    ""

and check_iter dec check incr stats parent_sym_tabl =
  let st1 = make_symbol_table parent_sym_tabl in
  (check_expression dec st1)
  st2 = make_symbol_table st1 
  (check_expression check st2)
  st3 = make_symbol_table st2
  (check_statements stats st3)
  st4 = make_symbol_table st3
  (check_expression incr st4)
  (* Check to make sure check is bool *)
  "" 
  
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
  | _ -> raise (Error "Malformed statement")

and check_statements statements sym_tabl =
  match statements with
    head::tail ->
      (check_statement head sym_tabl)
      (check_statements tail sym_tabl)
  | _ -> raise (Error "Malformed statement")

let generate_sast program =
  match program with
    Program(imp, stat) -> 
      global_table := make_symbol_table Nil;
  let sast = check_statements stat global_table in
    sast
;;
