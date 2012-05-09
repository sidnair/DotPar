open Ast;;
open Str;;

exception Error of string
exception Variable_not_defined of string

module StringMap = Map.Make(String);;

(* utility function *)

type debug_state_monad = { mutable debug_switch : bool };;
let debug_state = { debug_switch = true; };;

let debug str = if debug_state.debug_switch then print_string (str);;

let rec lookup id sym_table iter =
  debug("Looking for "^ id ^" ...\n");
  try
    let t = StringMap.find id sym_table.table in
    debug("Found " ^ id ^ " ...\n");
    (t, iter)
  with Not_found ->
    match sym_table.parent with
    | Some(parent) -> lookup id parent (iter +1 )
    | _ -> raise Not_found

let link_tables p_table c_table =
  debug("Linking a symbol_table... \n");
  c_table.parent <- Some(p_table);
  p_table.children <- c_table :: p_table.children;
  ()

let rec get_symbol_table id sym_table iter =
  debug("Looking for symbol_table based on "^ id ^" ...\n");
  try
    let _ = StringMap.find id sym_table.table in
    debug("Found " ^ id ^ " ...\n");
    (sym_table, iter)
  with Not_found ->
    match sym_table.parent with
    | Some(parent) -> get_symbol_table id parent (iter +1 )
    | _ -> raise Not_found

let get_fn_sym table name_expr =
  match name_expr with
  | Variable(name) ->
      let func_type, iter = lookup name table 0 in
      (match func_type with
      | Func_type(var_type, params, f_syms) -> !(f_syms)
      | _ -> raise (Error "Expected function - semantic analysis failed")
      )
  | Anonymous_function (var_type, params, stats, s_t) -> s_t
  | _ -> raise (Error "Expected function name - semantic analysis failed")
    
(***************************************************************************)
let rec check_expression e sym_tabl =
  debug("Checking an expression... \n");
  (match e with
  | Assignment_expression (left, right) ->
      let rec check_array_type expr depth = 
          (match expr with
          | Array_access(name, index) -> 
            ignore(check_expression index sym_tabl);
            (check_array_type name (depth + 1))
          | Variable(v) ->
            let (t, iter) = lookup v sym_tabl 0 in
            let t1 = check_expression right sym_tabl in
            ignore(compare_type t (aug_type t1 depth));
            t
          | _ -> raise (Error "sup"))
      and aug_type t depth =
        if (depth == 0) then t else (aug_type (Array_type(t)) (depth - 1))
      in
          let t1 = (check_expression right sym_tabl) in
          ignore(check_array_type left 0);
          t1
  | Declaration (var_type, var) ->
    (match var with
      | Variable(v) ->
        (try
          let typ, iter = lookup v sym_tabl 0 in
          if (iter > 0) then raise Not_found
          else begin
            raise (Error "Vannriable previously defined in the same scope");
          end
        with Not_found ->
          let t1 = check_var_type var_type in
          if (require_void t1) then raise
            (Error "Cannot declare a variable of type void")
          else begin
            ignore(add_to_symbol_table v t1 sym_tabl);
            t1
          end
        )
      | _ -> raise (Error "Invalid Declaration Type")
    )
  | Declaration_expression (var_type, var, right) ->
    (match var with
      | Variable(v) ->
          (try
            let typ, iter = lookup v sym_tabl 0 in
            if (iter > 0) then raise Not_found
            else begin
              raise (Error "Variable perviously defined in the same scope");
            end
          with Not_found ->
            let t = check_var_type var_type in
            let t2 = check_expression right sym_tabl in
            let t3 = (match t2 with
                  | Func_type(ret_type, params, sym_ref) -> ret_type
                  | _ as x -> x)
            in
            ignore(compare_type t t3);
            ignore(add_to_symbol_table v t sym_tabl);
            t3
          )
      | _ -> raise (Error "Malformed Declaration Expression"))
  | Array_literal (exprs) ->
    (if ((List.length exprs) = 0) then Array_type (Basic_type Void_type)
     else Array_type((check_expression (List.nth exprs 0) sym_tabl)))
  | List_comprehension (expr, params, exprs, expr1, symbol_table) ->
    ignore(link_tables sym_tabl symbol_table);
    let check_param_table param = check_param param symbol_table in
    let check_exp_table e =
      match (check_expression e symbol_table) with
      | Array_type(a) -> a
      | _ -> (check_expression e symbol_table)
    in
    ignore(List.map2 compare_type
      (List.map check_param_table params)
      (List.map check_exp_table exprs));
    ignore(compare_type (check_param  (List.hd params) symbol_table)
    (check_expression expr symbol_table));

    let filter  = check_expression expr1 symbol_table in
    debug(repr_of_type "  " filter);
    if (require_bool filter || require_void filter || require_func filter )     then  (*returning *) Array_type(check_expression expr symbol_table)
    else begin
      raise (Error "Bad filter in List Comprehension")
    end
  | Unop (op, expr) ->
    let t = (check_expression expr sym_tabl) in
    ignore(check_unop op t);
    (get_type (Unop(op, expr)) sym_tabl)
  | Binop (expr, op, expr1) ->
    debug("checkin binop");
    let t = (check_expression expr sym_tabl) in
    let t2 = (check_expression expr1 sym_tabl) in
    ignore(check_operator t op t2);
    (get_type (Binop(expr, op, expr1)) sym_tabl)
  | Function_call (expr, exprs) ->
    debug("mathced on function callllll");
    let get_type_table expr =
        check_expression expr sym_tabl
    in
    (match expr with
      | Variable(v) ->
          (try
            let (t, iter) = lookup v sym_tabl 0 in
            (match t with
            | Func_type(ret_type, var_types, sym_ref) ->
              ignore (List.map2 compare_type var_types
              (List.map get_type_table exprs));
            ret_type
            | _ -> raise (Error "Invalid params in func call"))
          with Not_found -> raise (Error "Function not found"))
      | _ as x -> (check_expression x sym_tabl))
  | Array_access (name, index) ->
    debug("Matched on array access");
    (match name with
      | Variable(v) ->
        let (t, iter) = lookup v sym_tabl 0 in
        let index_t = check_expression index sym_tabl in
        if not (require_number index_t)
          then raise (Error "Invalid Array Access")
        else begin
            (match t with
            | Array_type(a) -> a
            | _ -> t)
      | _ -> raise (Error "sdfsdfMalformed Array Statement111"))
  | Variable (v) -> let (t, expr) = lookup v sym_tabl 0 in t
  | Char_literal (c) -> Basic_type(Char_type)
  | Number_literal (f) -> Basic_type(Number_type)
  | String_literal (str) -> Array_type(Basic_type(Char_type))
  | Boolean_literal (b) -> Basic_type(Boolean_type)
  | Nil_literal -> Basic_type(Void_type)
  | Anonymous_function (var_type, params, stats, s_t) ->
       debug("Matched on Anonymous Function");
      ignore(link_tables sym_tabl s_t);
      check_func_def "anon" var_type params stats s_t sym_tabl
  | Function_expression (stat) ->
      debug("Matched on Function Expression");
      (match stat with
      | Function_definition(name, ret_type, params, sts, symbol_table) ->
        ignore(link_tables sym_tabl symbol_table);
        ignore(check_func_def name ret_type params sts symbol_table sym_tabl);
        let _ = (check_var_type ret_type) in
        ret_type
      | _ -> raise (Error "Malformed Function expression"))
  | Empty_expression -> Basic_type(Void_type)
  )

and get_type expression sym_tabl =
  (match expression with
  | Array_literal (exprs) ->
      (if ((List.length exprs) = 0) then
        Array_type (Basic_type Void_type)
      else
        Array_type((get_type (List.nth exprs 0) sym_tabl)))
  | List_comprehension (expr, params, exprs, expr1, s_t) ->
      ignore(link_tables sym_tabl s_t);
      (get_type expr sym_tabl)
        (* assume the subexpressions match *)
  | Unop (op, expr) ->
      (match op with
      | Neg -> Basic_type(Number_type)
      | Not -> Basic_type(Boolean_type))
  | Binop (expr, op, expr1) ->
      (match op with
        Add -> Basic_type(Number_type)
      | Sub -> Basic_type(Number_type)
      | Mult -> Basic_type(Number_type)
      | Div -> Basic_type(Number_type)
      | Mod -> Basic_type(Number_type)
            (* these relational guys shouldn't be just numbers !!! *)
      | Eq -> Basic_type(Boolean_type)
      | Neq -> Basic_type(Boolean_type)
      | Lt -> Basic_type(Boolean_type)
      | Leq -> Basic_type(Boolean_type)
      | Gt ->  Basic_type(Boolean_type)
      | Geq -> Basic_type(Boolean_type)
      | And -> Basic_type(Boolean_type)
      | Or -> Basic_type(Boolean_type))
        (* extract the return value *)
  | Function_call (expr, exprs) -> (* (get_type expr ) *)
      let get_type_table t = get_type t sym_tabl in
      Func_type((get_type expr sym_tabl)
      , (List.map get_type_table exprs)
      , (ref (make_symbol_table (Some(sym_tabl)));))
      (* strip a layer off *)
  | Array_access (name, index) ->
      (match name with
        | Variable(v) ->
          let (t, iter) = lookup v sym_tabl 0 in
          let index_t = get_type index sym_tabl in
          if not (require_number index_t)
            then raise (Error "Invalid Array Access")
          else begin
            (match t with
            | Array_type(a) -> a
            | _ -> t)
          end
        | _ -> raise (Error "Invalid Array Name"))
  | Variable (v) -> (fst (lookup v sym_tabl 0))
  | Char_literal (c) -> Basic_type(Char_type)
  | Number_literal (f) -> Basic_type(Number_type)
  | String_literal (str) -> Array_type(Basic_type(Char_type))
  | Boolean_literal (b) -> Basic_type(Boolean_type)
  | Anonymous_function (var_type, params, stats, s_t) ->
      let check_params_table param = check_param param sym_tabl in
      Func_type((check_var_type var_type),
        (List.map check_params_table params), (ref (make_symbol_table
        (Some(sym_tabl)));))
  | Function_expression (stat) ->
      (match stat with
      | Function_definition(name, ret_type, params, sts, symbol_table) ->
        let check_params_table param = check_param param sym_tabl in
        Func_type((check_var_type ret_type),
        (List.map check_params_table params), (ref (make_symbol_table
        (Some(sym_tabl)))))
      | _ -> raise (Error "Function expression invalid"))
  | Empty_expression -> Basic_type(Void_type)
  | _ -> raise (Error "Error in Semantic Analysis. Unknown Type Found")
  )

and compare_type type1 type2 =
  let catch_func t =
    match t with
    | Func_type(ret_type, params, sym_tabl)-> true
    | _ -> false
    in
  debug ("Comparing two types...\n" ^ (repr_of_type " " type1) ^ "\n"
  ^ (repr_of_type " " type2) ^ "\n");
  (* specially handle empty arrays *)
  let array_void_any t =
    let rec array_void_any_rec t =
      match t with
      | Array_type(t) -> array_void_any_rec(t)
      | Any_type -> true
      | Basic_type(b) -> (match b with Void_type -> true | _ -> false)
      | _ -> false
    in
    match t with
    | Array_type(t) -> array_void_any_rec(t)
    | _ -> false
  in
  let rec array_layers a1 a2 =
  match a1 with
  | Array_type(b1) ->
      (match a2 with
      | Array_type(b2) -> (array_layers b1 b2)
      | _ -> raise (Error "Mismatched array types 1"))
  | Any_type ->
    (match a2 with
      | Any_type -> raise (Error "Mismatched array types 2")
      | _ -> true)
  | _ ->
      (match a2 with
      | Array_type(b2) -> raise (Error "Mismatched array types 3")
      | Any_type -> true
      | _ -> false)
  in
  let any = false in
   if ((array_void_any type1) && (array_void_any type2)) then
    raise (Error "Interior type could not be determined")
  else if (array_void_any type1) then begin
     ignore(any = (if (array_layers type1 type2) then true
     else false));
     type2
  end
  else if (array_void_any type2) then begin
    ignore(any = (if (array_layers type1 type2) then true
     else false));
     type1
  end
  (* check for catchall types *)
  else if (type1 = Any_type) then
    type2
  else if (type2 = Any_type) then
    type1
   (*custom validiation for func types, we need to ignore symbol tables*)
  else if ((catch_func type1) && (catch_func type2)) then begin
    let rec determine_equal t1 t2 =
       match t1 with
        | Func_type(rt, params, sym_tabl) ->
            (match t2 with
             | Func_type(rt2, params2, sym_tabl2) ->
                ignore(compare_type rt rt2);
                ignore(List.map2 compare_type params params2);
             | _ -> ())
        | _ -> ()
    in
    (determine_equal type1 type2);
    type1
    end
    (* do vanilla type checking here *)
  else if ((not (type1 = type2)) && (not any)) then raise
      (Error (Printf.sprintf "Type Mismatch: got %s expected %s"
      (string_of_type type1) (string_of_type type2)))
  else type1

and check_unop op type1 =
  ignore(debug ("Checking a unop ...\n"));
  (match op with
    Neg -> if not (require_number type1)
            then raise (Error "Operator applied invalid type") else ""
  | Not -> if not (require_bool type1)
            then raise (Error "Operator applied to invalid type") else "")

and check_operator type1 op type2 =
  debug ("Checking an operator...\n");
  (match op with
    Add ->
      if not ((require_number type1) && (require_number type2))
      then raise (Error "Operator applied invalid type")
  | Sub ->
      if not ((require_number type1) && (require_number type2))
      then raise (Error "Operator applied invalid type")
  | Mult ->
      if not ((require_number type1) && (require_number type2))
      then raise (Error "Operator applied invalid type")
  | Div ->
      if not ((require_number type1) && (require_number type2))
      then raise (Error "Operator applied invalid type")
  | Mod ->
      if not ((require_number type1) && (require_number type2))
      then raise (Error "Operator applied invalid type")
  | Eq ->
      if not (type1 = type2)
      then raise (Error "Types do not match on both sides of the operator")
  | Neq ->
      if not (type1 = type2)
      then raise (Error "Types do not match on both sides of the operator")
  | Lt ->
      if not ((require_number type1) && (require_number type2))
      then raise (Error "Operator applied invalid type")
  | Leq ->
      if not ((require_number type1) && (require_number type2))
      then raise (Error "Operator applied invalid type")
  | Gt ->
      if not ((require_number type1) && (require_number type2))
      then raise (Error "Operator applied invalid type")
  | Geq ->
      if not ((require_number type1) && (require_number type2))
      then raise (Error "Operator applied invalid type")
  | And ->
      if not ((require_bool type1) && (require_bool type2))
      then raise (Error "Operator applied invalid type")
  | Or ->
      if not ((require_bool type1) && (require_bool type2))
      then raise (Error "Operator applied invalid type"))
  (*| _ -> raise (Error "Unsupported Binary Operator")*)


and check_basic_type btype =
  debug("Checking basic type...\n");
  match btype with
    Void_type -> Void_type
  | Number_type -> Number_type
  | Char_type -> Char_type
  | Boolean_type -> Boolean_type
  (*| _ -> raise (Error "Unsupported Type")*)

and check_var_type var_type =
  debug("Checking var_types...\n");
  match var_type with
  | Basic_type(b) -> Basic_type((check_basic_type b))
  | Array_type(a) ->
      let rec build_array t1 iter =
        if (iter = 0) then t1 else
          (build_array (Array_type(t1)) (iter - 1));
      in
      let rec det_array t1 iter =
        (match t1 with
        | Array_type(a1) -> det_array a1 (iter + 1)
        | Basic_type(b) ->
            (if (require_void (Basic_type(b))) then raise
            (Error "Cannot declare an array of type void")
            else (build_array (Basic_type(b)) iter))
        | _ -> raise (Error "Incorrect type found in array"))
       in
        let temp = (det_array a 0) in
        (Array_type(temp))
  | Func_type(ret_type, param_types, sym_ref) ->
      Func_type((check_var_type ret_type),
        (List.map check_var_type param_types), sym_ref)
  | Func_param_type(ret_type, params) ->
    raise (Error "Function paramater types not supported ")
  | _ -> raise (Error "Unsupported variable type")

and check_param param sym_tabl =
  debug ("Checking params...\n");
  match param with
    Param(param_type, varname) ->
      match varname with
      | Variable(v) ->
        (let t = check_var_type param_type in
        if (require_void t) then raise
            (Error "Cannot pass param of type void")
          else begin
            ignore(add_to_symbol_table v t sym_tabl);
            t
          end)
      | _ -> raise (Error "Param type invalid")

and check_boolean v =
  if not (require_bool v) then raise
    (Error "Type found where Boolean expected")
  else ()

and check_selection select sym_tabl =
  debug ("Checking a selection block... \n");
  ignore(link_tables sym_tabl select.if_sym_tabl);
  ignore(check_boolean (get_type select.if_cond sym_tabl));
  ignore(check_statements select.if_body select.if_sym_tabl);
  if ((List.length select.elif_conds) != 0) then
    let check_elif cond body s_t =
      ignore(link_tables sym_tabl s_t);
      ignore(check_boolean (get_type cond s_t));
      ignore(check_statements body s_t)
    in
    for i=0 to (List.length select.elif_conds)-1 do
      (check_elif
        (List.nth select.elif_conds i)
        (List.nth select.elif_bodies i)
        (List.nth select.elif_sym_tabl i));
    done;
  else begin
    if (List.length select.else_body) != 0 then
      (ignore(link_tables sym_tabl select.else_sym_tabl);
       ignore(check_statements select.else_body select.else_sym_tabl))
    else ()
  end

and check_iter dec check incr stats sym_tabl head_sym_tabl =
  (link_tables head_sym_tabl sym_tabl);
  ignore(check_expression dec head_sym_tabl);
  if not ( require_bool (get_type check head_sym_tabl)) then
    raise (Error "Conditonal in iteration not of type Boolean")
    else begin
      ignore(check_expression incr sym_tabl);
      ignore(check_statements stats sym_tabl);
    end

and check_func_def (name : string) ret_type params stats sym_tabl p_s_tabl =
  debug ("Checking a func def...\n");
  let check_local_scope a_name =
    let (t, iter) = lookup a_name p_s_tabl 0 in
    if (iter > 0) then raise Not_found
    else begin
      raise (Error "Function of same name previous declared in local scope");
    end
  in
  try
    if name = "anon" then raise Not_found
    else begin
      check_local_scope name
    end
  with Not_found ->
    let v = check_var_type ret_type in
    let check_param_table param =
      (check_param param sym_tabl) in
    ignore(add_to_symbol_table name
    (Func_type(v, (List.map check_param_table params), (ref sym_tabl))) p_s_tabl);
    let com_bools x y = x || y in
    let rec match_jump_types stat =
      debug ("Looking for jumps in " ^ name ^ "...\n");
    (match stat with
    | Statements(s) -> debug("Statements\n");
        List.fold_left com_bools false (List.map match_jump_types s)
    | Selection(s) -> debug("Selections\n");
        List.fold_left com_bools false (List.map match_jump_types
          (List.concat [s.if_body; s.else_body;
          (List.concat s.elif_bodies)]) )
    | Iteration(d,c,i,s, s_t, h_s_t) -> debug("Iteration\n");
        List.fold_left com_bools false (List.map match_jump_types s)
    | Jump(j) -> debug("Jumping!\n");
        ignore(compare_type (get_type j sym_tabl) v);
        true
    | Expression(e) -> debug("Expression\n");
        false
    | _ -> debug("Catch all\n");
        false)
    in
    let b =
      List.fold_left com_bools false (List.map match_jump_types stats)
    in
    if b then debug("True \n")
      else debug("False \n");
        if b && (require_void v) then raise
          (Error "Void method contains return")
        else
          if (not b) && not (require_void v) then
            raise (Error "Non-void method must contain return statement")
          else (Func_type(v, (List.map check_param_table params), (ref sym_tabl)))

and require_void type1 =
  match type1 with
  | Basic_type(b) ->
      (match b with
      | Void_type -> true
      | _ -> false)
  | _ -> false
and require_number type1 =
  match type1 with
  | Basic_type(b) ->
      (match b with
        | Number_type -> true
        | _ -> false)
  | _ -> false
and require_char type1 =
  match type1 with
  | Char_type -> true
  | _ -> false
and require_bool type1 =
  match type1 with
  | Basic_type(b) ->
      (match b with
        | Boolean_type -> true
        | _ -> false)
  | _ -> false
and require_func type1 =
  match type1 with
  | Func_type(e,p,s) -> true
  | _ -> false

and check_statement stat sym_tabl =
  debug ("Checking Statement... \n");
  match stat with
  | Expression(e) -> ignore (check_expression e sym_tabl);
  debug("Matched on Expression");
  | Statements(s) -> ignore (check_statements s sym_tabl);
  | Selection(s) -> ignore (check_selection s sym_tabl);
  debug("Matched on Selection");
  | Iteration(dec, check, incr, stats, symbol_table, head_sym_tab) ->
      debug("Matched on Iteration");
      ignore(link_tables sym_tabl head_sym_tab);
      ignore(check_iter dec check incr stats symbol_table head_sym_tab);
  | Jump(j) -> ignore (check_expression j sym_tabl);
      debug ("Check jump...\n");
  | Function_definition(name, ret_type, params, sts, symbol_table) ->
      ignore(link_tables sym_tabl symbol_table);
      ignore(check_func_def name ret_type params sts symbol_table sym_tabl);
      ignore(check_statements sts symbol_table);

and check_statements stats sym_tabl =
  debug "Checking Statements... \n";
  match stats with
  | hd :: tl ->
      ignore(check_statement hd sym_tabl);
      ignore(check_statements tl sym_tabl);
  | [] -> ()

let generate_sast program debug =
  ignore(debug_state.debug_switch <- debug);
  match program with
  | Program(imp, stat, symbol_table) ->
      ignore(check_statements stat symbol_table);
      (* check if we have a main *)
      ignore(try
        let main_type = (fst (lookup "main" symbol_table 0)) in
        (* check if main has the right types *)
        match main_type with
        | Func_type(ret_type, params, sym_ref) ->
            (if not (ret_type = (Basic_type Void_type)) then
              raise (Error "Main return needs to be void"));
            ignore(match (List.length params) with
            | 0 -> ignore(true)
            | 1 ->
                ignore(let param = (List.nth params 0) in
                if not (param = Array_type(Basic_type Char_type)) then
                  raise (Error "Parameter does not have correct type"))
            | _ -> raise (Error "Too many parameters"));
        | _ -> raise (Error "Main needs to be function")
      with Not_found ->
      raise (Error "Need a main function"));
      (* rebuild the ast? *)
      Program(imp, stat, symbol_table)
