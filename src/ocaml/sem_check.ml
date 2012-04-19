(* translate into scala *)
open Ast
open Builtin (* to do *)
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
  let table = List.fold_left (fun tableMaker (id, element) -> StringMap.add id element tableMaker) StringMap.empty Builtin.get in
  let global_sym_table = { init = table; parent = None }

(* utility function *)
let check_types  = function
    ValType(Float), ValType(Float) -> Some(Float)
  | _ -> None

(* lookup in string table *)
let rec lookup_table table id =
  try
    StringMap.find id table.table 
  with Not_found  ->
    match table.parent with
      Some(p) -> lookup_table p id
    | None    -> raise (Symbol_undefined id)

(* Build string table *)


(* binary operator *)
let rec eval = function
    Lit(x) -> x
    | Binop(e1, op, e2) ->
        let v1 = eval e1 and v2 = eval e2 in
        match op with
        Add -> v1 + v2
        | Sub -> v1 - v2
        | Mul -> v1 * v2
        | Div -> v1 / v2
