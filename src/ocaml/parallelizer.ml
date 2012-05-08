open Ast;;
open Semantic;;

let rec can_par_statements statements symbols =
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

and db_par name is_pure =
  let pure_string_of_bool b =
    if b then "pure" else "impure"
  in
  Printf.printf "%s: %s\n" name (pure_string_of_bool is_pure)

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
      Program(imports, statements, symbols)
