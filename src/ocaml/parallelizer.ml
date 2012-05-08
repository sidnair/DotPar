open Ast;;
open Semantic;;

type debug_state_monad = { mutable debug_switch : bool };;
let debug_state = { debug_switch = false; };;
let debug str = if debug_state.debug_switch then print_string (str);;

let rec are_assoc_statements statements symbols =
  match statements with
  | head::tail ->
      let can_assoc_head = is_assoc_statement head symbols in
      let can_assoc_tail = are_assoc_statements tail symbols in
      can_assoc_head && can_assoc_tail
  | [] -> true

and is_assoc_statement statement symbols =
  match statement with
  | Expression(e) -> is_assoc_expr e
  | Statements(s) -> are_assoc_statements s symbols
  | Jump(e) -> is_assoc_expr e
  (* Don't recurse on functions defined in if statements or for loops *)
  | Selection(s) -> false
  | Iteration(e1, e2, e3, stmts, table, head_table) -> false
  | Function_definition(name, ret_type, params, stmts, table) ->
      let is_assoc = are_assoc_statements stmts table in
      table.associative <- is_assoc;
      db_assoc name is_assoc;
      (* Regardless of whether or not the function is associative, the
       * definition does not violate purity. *)
      true

and is_assoc_expr expr =
  match expr with
  | (Binop(e1, Add, e2)
    | Binop(e1, Mult, e2)
    | Binop(e1, And, e2)
    | Binop(e1, Or, e2)) -> (is_assoc_expr e1) && (is_assoc_expr e2)
  | Declaration(type_dec, e1) -> true
  | Array_literal(exprs) -> true
  | Array_access(e1, e2) -> true
  | Variable(str) -> true
  | Char_literal(c) -> true
  | Number_literal(n) -> true
  | String_literal(s) -> true
  | Boolean_literal(b) -> true
  | Nil_literal -> true
  | Empty_expression -> true
  | _ -> false

and can_par_statements statements symbols =
  match statements with
  | head::tail ->
      let can_par_head = can_par_statement head symbols in
      let can_par_tail = can_par_statements tail symbols in
      can_par_head && can_par_tail
  | [] -> true

and can_par_statement statement symbols =
  match statement with
  | Expression(e) -> can_par_expr e symbols
  | Statements(s) -> can_par_statements s symbols
  | Selection(s) -> can_par_selection s symbols
  | Iteration(e1, e2, e3, stmts, table, head_table) ->
      (* This only checks the head statements if the body fails the test. *)
      let is_pure = can_par_statements stmts table &&
          can_par_expr e1 head_table &&
          can_par_expr e2 head_table &&
          can_par_expr e3 head_table
      in
      table.pure <- is_pure;
      db_par "for" is_pure;
      is_pure
  | Jump(e) -> can_par_expr e symbols
  | Function_definition(name, ret_type, params, stmts, table) ->
      let is_pure = can_par_statements stmts table in
      table.pure <- is_pure;
      db_par name is_pure;
      (* Regardless of whether or not the function is pure, the definition
       * does not violate purity. *)
      true

and db_assoc name is_assoc =
  let assoc_string_of_bool b =
    if b then "assoc" else "non-assoc"
  in
  debug (Printf.sprintf "%s: %s\n" name (assoc_string_of_bool is_assoc))

and db_par name is_pure =
  let pure_string_of_bool b =
    if b then "pure" else "impure"
  in
  debug (Printf.sprintf "%s: %s\n" name (pure_string_of_bool is_pure))

(* Currently, parallelization of an if statement or else statement is not
 * supported. The symbol tables here must be updated to enable that if
 * desired.*)
and can_par_selection stmt symbols =
  let can_par_if = can_par_expr stmt.if_cond symbols in
  let can_par_if_body = can_par_statements stmt.if_body stmt.if_sym_tabl in
  let can_par_else_body = can_par_statements stmt.else_body stmt.else_sym_tabl in
  let can_par_rest =
      can_par_elifs stmt.elif_conds stmt.elif_bodies stmt.elif_sym_tabl symbols
  in
  can_par_if && can_par_if_body && can_par_else_body && can_par_rest

and can_par_elifs conds bodies tables outer_table =
  if ((List.length conds) == 0) then
    true
  else
    let can_par_conds = can_par_expr (List.hd conds) outer_table in
    let can_par_body = can_par_statements (List.hd bodies) (List.hd tables) in
    let can_par_rest =
      can_par_elifs (List.tl conds) (List.tl bodies) (List.tl tables) outer_table
    in
    can_par_conds && can_par_body && can_par_rest

and can_par_expr e symbols =
  match e with
  | Assignment_expression(lv, rv) -> can_par_assignment_expr lv symbols
  | List_comprehension(expr, params, exprs, if_expr, table) ->
      (* if_expr should be tiny, so only check it if the main expression is
       * pure *)
      let is_pure = can_par_expr expr table && can_par_expr if_expr table
      in
      table.pure <- is_pure;
      db_par "list_comp" is_pure;
      is_pure
  (* TODO: detect our own functions are pure for free by looking it up in our
   * symbol table; functions which we can statically identify could be true. *)
  | Function_call(expr, exprs) -> false
  | Anonymous_function(type_def, params, stmts, table) ->
      let is_pure = can_par_statements stmts table in
      table.pure <- is_pure;
      db_par "anon" is_pure;
      true
  (* Can roll these all into one case, but keep it explicit for now. *)
  (* TODO: is it necessary to recurse into these statements? *)
  | Function_expression(state) -> true
  | Declaration(type_dec, expr) -> true
  | Declaration_expression(type_dec, rv, lv) -> true
  | Array_literal(exprs) -> true
  | Unop(op, expr) -> true
  | Binop(expr1, op, expr2) -> true
  | Array_access(expr, expr2) -> true
  | Variable(str) -> true
  | Char_literal(c) -> true
  | Number_literal(n) -> true
  | String_literal(s) -> true
  | Boolean_literal(b) -> true
  | Nil_literal -> true
  | Empty_expression -> true

and can_par_assignment_expr lv symbols =
  match lv with
  | Variable(name) -> snd(lookup name symbols 0) == 0
  | Array_access(e1, e2) -> (match e1 with
      | Variable(name) -> snd(lookup name symbols 0) == 0
      | _ -> raise (Error "Malformed array statement")
      )
  | _ -> raise (Error "Invalid assignment expression")

let parallelize program =
  match program with
  | Program(imports, statements, symbols) ->
      ignore(can_par_statements statements symbols);
      ignore(are_assoc_statements statements symbols);
      Program(imports, statements, symbols)
