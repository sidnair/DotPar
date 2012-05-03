open Ast;;
open Str;;

exception Error of string
exception Variable_not_defined of string

module StringMap = Map.Make(String);;

type symbol_table = { 
  mutable table : string StringMap.t;
  mutable parent: symbol_table option; 
  mutable children : symbol_table list;
  mutable pure : bool
} 

let rec lookup id sym_table iter =  
  try 
    let t = StringMap.find id sym_table.table in 
    (t, iter) 
  with Not_found ->
    match sym_table.parent with 
    | Some(parent) -> lookup id parent (iter +1 )
    | _ -> raise Not_found 

let add_to_symbol_table id id_type sym_table = 
  sym_table.table <- StringMap.add id id_type sym_table.table;
  ()
  
let make_symbol_table p_table = 
  let s_table = {
    table = StringMap.empty;
    parent = Some(p_table);
    children = [];
    pure = false;
  } in
  ignore(p_table.children <- s_table :: p_table.children);
  s_table 

(* This method is very experimental *)
(*let rec get_symbol_table root id iter = *)
  (*let rec wrap_get_sym_table root id iter tail = *)
    (*(try ignore(StringMap.find id root.table);*)
      (*root*)
    (*with Not_found ->*)
      (*(match tail with *)
      (*| [] ->    *)
      (*| h :: tl -> wrap_get_sym_table h id (iter+1) tl))*)
  (*in*)
  (*try*)
    (*ignore(StringMap.find id root.table);*)
    (*root*)
  (*with Not_found ->*)
    (*match root.children with*)
    (*| [] -> raise (Not_found)*)
    (*| h :: tl -> wrap_get_sym_table h id (iter+1) tl*)

let ht = Hashtbl.create 100;;
(***************************************************************************)
let rec check_expression e sym_tabl = 
  (match e with 
  | Assignment_expression (left, right) ->
    (match left with
      | Variable(v) ->
        let (t, iter) = lookup v sym_tabl 0 in
        let t1 = get_type right sym_tabl in
        ignore(compare_type t t1);
        t1 
      | Array_access(name, index) ->
        (match name with 
          | Variable(v) -> 
            let (t, iter) = lookup v sym_tabl 0 in
            let index_t = get_type index sym_tabl in
            if index_t <> "Number" then raise (Error "Invalid Array Access")
            else begin 
              let t1 = get_type right sym_tabl in
              ignore(compare_type t t1);
              t1
            end
          | _ -> raise (Error "Malformed Array Statement"))
       | _ -> raise (Error "Invalid Assignment Expression")
    )
  | Declaration (var_type, var) ->
    (match var with
      | Variable(v) ->
        (try
          ignore(lookup v sym_tabl 0);
          raise (Error "Variable previously defined");
        with Not_found ->
          let t1 = check_var_type var_type in
          ignore(add_to_symbol_table v t1 sym_tabl);
          t1 
        )
      | _ -> raise (Error "Invalid Declaration Type")
    ) 
  | Declaration_expression (var_type, var, right) ->
    (match var with
      | Variable(v) -> 
          (try
            ignore(lookup v sym_tabl 0);
            raise (Error "Variable perviously defined");
          with Not_found ->
            let t = check_var_type var_type in
            let t2 = get_type right sym_tabl in
            ignore(compare_type t t2);
            ignore(add_to_symbol_table v t sym_tabl);
            t2
          )
      | _ -> raise (Error "Malformed Declaration Expression"))
  | Array_literal (exprs) ->
    let get_type_wrap expr = 
      get_type expr sym_tabl
    in
    let t = get_type (List.hd exprs) sym_tabl in
    ignore (List.fold_left compare_type t (List.map get_type_wrap exprs));
    t
  (* TODO *)
  | List_comprehension (expr, params, exprs, expr1) -> ""
  | Unop (op, expr) ->
      let t = (get_type expr sym_tabl) in
      ignore(check_unop op t);
      t
  | Binop (expr, op, expr1) -> 
      let t = (get_type expr sym_tabl) in
      let t2 = (get_type expr1 sym_tabl) in
      ignore(check_operator t op t2);
      t2
  | Function_call (expr, exprs) ->
    let get_type_table expr = 
      get_type expr sym_tabl
    in 
    (match expr with
      | Variable(v) ->
          (try
            let (t, iter) = lookup v sym_tabl 0 in 
          ignore (List.map2 compare_type 
                  (Hashtbl.find ht v)
                  (List.map get_type_table exprs));
          t
          with Not_found -> raise (Error "Function not found")
          ) 
      | _ -> raise (Error "Malformed function call")) 
  | Array_access (name, right) -> 
    (match name with 
      | Variable(v) -> 
        let (t, iter) = lookup v sym_tabl 0 in
        let index_t = get_type right sym_tabl in
        if index_t <> "Number" then raise (Error "Invalid Array Access")
        else begin 
        t
        end
      | _ -> raise (Error "Malformed Array Statement"))
  | Variable (v) -> let (t, expr) = lookup v sym_tabl 0 in t 
  | Char_literal (c) -> "Char" 
  | Number_literal (f) -> "Number"
  | String_literal (str) -> "String"
  | Boolean_literal (b) -> "Boolean"
  | Nil_literal -> "Nil"
  | Anonymous_function (var_type, params, stats) ->
      let check_param_table param = check_param param sym_tabl in 
      ignore(List.map check_param_table params);
      check_var_type var_type
      (* TODO Match Jump Statement with Return Type *)
  | Function_expression (stat) ->
      (match stat with
      | Function_definition(name, ret_type, params, sts) ->
      ignore(check_func_def name ret_type params sts sym_tabl);
      let t = (check_var_type ret_type) in 
      t
      | _ -> raise (Error "Malformed Function expression"))
  | Empty_expression -> ""
  )

and get_type expression sym_tabl =
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

and compare_type type1 type2 =
  if type1 <> type2 then raise (Error "Type Mismatch")
  else type1

and check_unop op type1 =
  match op with
    Neg -> if (type1 <> "Number") 
            then raise (Error "Operator applied invalid type") else ""
  | Not -> if (type1 <> "Boolean")
            then raise (Error "Operator applied to invalid type") else ""

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
  (*| _ -> raise (Error "Unsupported Binary Operator")*)

and check_basic_type btype =
  match btype with
    Void_type -> "Void"
  | Number_type -> "Number"
  | Char_type -> "Char"
  | Boolean_type -> "Boolean"
  (*| _ -> raise (Error "Unsupported Type")*)

and check_var_type var_type : string =
  match var_type with
    Basic_type(b) -> (check_basic_type b)
  | Array_type(a) -> (check_var_type a)  ^ "[]"
  | Func_type(ret_type, param_types) ->
      ignore(List.map check_var_type param_types);
      (check_var_type ret_type);
 (* TODO
  * | Func_param_type(ret_type, params) ->*)
    (*let extract_type param = *)
      (*match param with*)
        (*Param(param_type, varname) -> param_type*)
    (*in*)
    (*let type_list = (List.map extract_type params) in*)
    (*(check_var_type ret_type)*)
    (*(List.map check_param params)*)
  | _ -> raise (Error "Unsupported variable type")

and check_param parm sym_tabl = 
  match parm with
    Param(param_type, varname) ->
      match varname with
      | Variable(v) ->
        let t = check_var_type param_type in
        ignore(add_to_symbol_table v t sym_tabl);
        t
      | _ -> raise (Error "Param type invalid")

and check_boolean v = 
  match v with 
  | "Boolean" -> ""
  | _ -> raise (Error "Type found where Boolean expected")

and check_selection select sym_tabl = 
  ignore(check_boolean (check_expression select.if_cond sym_tabl));
  ignore(check_statements select.if_body (make_symbol_table sym_tabl));
  if ((List.length select.elif_conds) != 0) then
    let check_elif cond body = 
      ignore(check_boolean (check_expression select.if_cond sym_tabl));
      ignore(check_statements body (make_symbol_table sym_tabl));
      "" 
    in
    ignore(List.map2 check_elif select.elif_conds select.elif_bodies);
  else begin 
    if (List.length select.else_body) != 0 then 
      ignore(check_statements select.else_body (make_symbol_table sym_tabl)) 
    else () 
  end

and check_iter dec check incr stats sym_tabl = 
  ignore(check_expression dec sym_tabl); 
  if ( (check_expression check sym_tabl) <> "Boolean") then 
    raise (Error "Conditonal in iteration not of type Boolean")
  else ""; 
  if ((check_expression incr sym_tabl) <> "Number") then
    raise (Error "Increment in iteration is not of type Number")
  else ""; 
  ignore(check_statements stats (make_symbol_table sym_tabl));

(* TODO 
 * Need to check function type matchs used return type *)
and check_func_def (name : string) ret_type params stats sym_tabl =
  try
    ignore (lookup name sym_tabl 0);
    raise (Error "Function previously declared")
  with Not_found ->
    let v = check_var_type ret_type in
    ignore(add_to_symbol_table name v sym_tabl); 
    let check_param_table param = check_param param sym_tabl in
    (Hashtbl.add ht name (List.map check_param_table params));
   
    (* TODO bunch of nested matches *)  
    ignore(check_statements stats (make_symbol_table sym_tabl));

and check_statement stat sym_tabl = 
  match stat with
  | Expression(e) -> ignore (check_expression e sym_tabl);
  | Statements(s) -> ignore (check_statements s sym_tabl);
  | Selection(s) -> ignore (check_selection s sym_tabl);
  | Iteration(dec, check, incr, stats) -> 
      ignore(check_iter dec check incr stats sym_tabl);
  | Jump(j) -> ignore (check_expression j sym_tabl);
  | Function_definition(name, ret_type, params, sts) ->
      ignore(check_func_def name ret_type params sts sym_tabl);
  (*| _ -> raise (Error "Malformed statement")*)

and check_statements stats sym_tabl =
  match stats with 
  | hd :: tl -> 
      ignore(check_statement hd sym_tabl);
      ignore(check_statements tl sym_tabl);
  | _ -> raise (Error "Poorly formed statement")

let generate_sast program = 
  match program with
  | Program(imp, stat) -> 
    let s_table = {
      table = StringMap.empty;
      parent = None;
      children = [];
      pure = false;
    } in
    ignore(check_statements stat s_table)
;;
