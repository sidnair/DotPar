open Ast;;
open Str;;

exception Error of string

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
    (*try ignore(StringMap.find id root.table);*)
      (*root*)
    (*with Not_found ->*)
      (*match tail with *)
      (*| [] -> ()   *)
      (*| h :: tl -> wrap_get_sym_table h id (iter+1) tl*)
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
let rec check_expression e sym_tabl = "" 

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
 (*| Func_param_type(ret_type, params) ->*)
    (*let extract_type param = *)
      (*match param with*)
        (*Param(param_type, varname) -> param_type*)
    (*in*)
    (*let type_list = (List.map extract_type params) in*)
    (*(check_var_type ret_type)*)
    (*(List.map check_param params)*)
  | _ -> raise (Error "Unsupported variable type")

and check_selection s sym_tabl = "" 

and check_iter dec check incr stats sym_tabl = 
  ignore(check_expression dec sym_tabl);
  if ( (check_expression check sym_tabl) <> "Boolean") then 
    raise (Error "Conditonal in iteration not of type Boolean")
  else ""; 
  
  ignore(check_statements stats (make_symbol_table sym_tabl));
 
  if ((check_expression incr sym_tabl) <> "Number") then
    raise (Error "Increment in iteration is not of type Number")
  else ""; 

and check_param parm sym_tabl = ""


(* Need to check function type matchs used return type *)
and check_func_def (name : string) ret_type params stats sym_tabl =
  try
    ignore (lookup name sym_tabl 0);
    raise (Error "Function previously declared")
  with Not_found ->
    let v = check_var_type ret_type in
    ignore(add_to_symbol_table name v sym_tabl); 
    let check_sym_params param  =
      check_param param sym_tabl in
    (Hashtbl.add ht name (List.map check_sym_params params));
   
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
