open Ast;;
open Semantic;;

let rec can_par_statements statements symbols =
  match statements with
  | head::tail ->
      (can_par_statement head symbols) && (can_par_statements tail symbols)
  | [] -> true

and can_par_statement statement symbols =
  match statement with
  | Expression(e) -> can_par_expr e symbols
  | Statements(s) -> can_par_statements s symbols
  | Selection(s) -> can_par_selection s symbols
  | Iteration(e1, e2, e3, stmts, table) ->
      let is_pure = can_par_expr e1 table &&
          can_par_expr e2 table &&
          can_par_expr e3 table &&
          can_par_statements stmts table
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
  Printf.printf "%s : %s\n" name (string_of_bool is_pure)

(* Currently, parallelization of an if statement or else statement is not
 * supported. The symbol tables here must be updated to enable that if
 * desired.*)
and can_par_selection stmt symbols =
  can_par_expr stmt.if_cond symbols &&
  can_par_statements stmt.if_body stmt.if_sym_tabl &&
  can_par_statements stmt.else_body stmt.else_sym_tabl &&
  can_par_elifs stmt.elif_conds stmt.elif_bodies stmt.elif_sym_tabl symbols

and can_par_elifs conds bodies tables outer_table =
  (List.length conds == 0) ||
  (can_par_expr (List.hd conds) outer_table &&
    can_par_statements (List.hd bodies) (List.hd tables) &&
    can_par_elifs (List.tl conds) (List.tl bodies) (List.tl tables) outer_table)

and can_par_expr e symbols =
  match e with
  | Assignment_expression(lv, rv) -> can_par_assignment_expr lv symbols
  (* TODO: can't parallelize this without array parallelization logic. *)
  | List_comprehension(expr, params, exprs, if_cond, table) -> false
  (* TODO: might be able to detect our own functions are pure for free by
   * looking it up in our symbol table; function variables would be false,
   * functinos we know the body of could be true. *)
  | Function_call(expr, exprs) -> false
  | Anonymous_function(type_def, params, stmts, table) ->
      let is_pure = can_par_statements stmts table in
      table.pure <- is_pure;
      db_par "anon" is_pure;
      true
  (* Can roll these all into one case, but keep it explicit for now. *)
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
