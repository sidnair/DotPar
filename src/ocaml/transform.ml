(* Transform the AST so all the lists point the right way *)
(* reverse:
   imports
   statements
   parameter lists (including list comprehensions)
   argument lists
   elif order
   array literals
  
   fixed array sizes are okay
*)

open Ast;;

let rec reverse_tree tree =
  match tree with
    Program(imports, statements) ->
      Program (reverse_imports(imports), reverse_statements(statements))

and reverse_imports imports =
  List.rev imports

and reverse_statements statements =
  let statements = List.rev statements
  in
  List.map reverse_statement statements
and reverse_statement statement =
  match statement with
    Expression(e) -> Expression (reverse_expression e)
  | Statements(s) -> Statements (reverse_statements s)
  | Selection(s) -> Selection (reverse_selection s)
  | Iteration(dec,check,incr, stats) ->
      Iteration (reverse_expression dec,
                 reverse_expression check,
                 reverse_expression incr,
                 reverse_statements stats)
  | Function_definition(name, ret_type, params, sts) ->
      Function_definition (name, reverse_type ret_type, List.rev params,
                           reverse_statements sts)
  | anything -> anything

and reverse_expressions exprs =
  let rev_exprs = List.map reverse_expression exprs in
  List.rev rev_exprs

and reverse_expression expr =
  match expr with
    Assignment_expression(e1, e2) ->
      Assignment_expression (reverse_expression e1, reverse_expression e2)
  | Declaration(var_type, expr) ->
      Declaration (reverse_type var_type, reverse_expression expr)
  | Declaration_expression(var_type, e1, e2) ->
      Declaration_expression (reverse_type var_type,
                              reverse_expression e1,
                              reverse_expression e2)
  | Array_literal(exprs) ->
      Array_literal (List.rev (List.map reverse_expression exprs))
  | List_comprehension(expr, params, exprs, if_cond) ->
      List_comprehension (reverse_expression expr,
                          List.rev params,
                          reverse_expressions exprs,
                          reverse_expression if_cond)
  | Unop(op, expr) -> Unop (op, reverse_expression expr)
  | Binop(e1, op, e2) ->
      Binop (reverse_expression e1, op, reverse_expression e2)
  | Function_call(expr, exprs) ->
      Function_call (reverse_expression expr,
                     reverse_expressions exprs)
  | Array_access(e1, e2) ->
      Array_access (reverse_expression e1, reverse_expression e2)
      (* *)
  | Anonymous_function(vtype, params, stats) ->
      Anonymous_function (reverse_type vtype, List.rev params,
                          reverse_statements stats)
  | Function_expression(stat) ->
      Function_expression (reverse_statement stat)
  | anything -> anything

and reverse_selection select = {
  if_cond = reverse_expression select.if_cond;
  if_body = reverse_statements select.if_body;
  else_body = reverse_statements select.else_body;
  elif_conds = reverse_expressions select.elif_conds;
  elif_bodies = 
  let stats = List.map reverse_statements select.elif_bodies in
  List.rev stats
 }

and reverse_type type_def = 
  match type_def with
  | Func_type(vt, vts) ->
      Func_type (vt, List.rev vts)
  | Func_param_type(vt, params) ->
      Func_param_type (vt, List.rev params)
  | anything -> anything
;;
