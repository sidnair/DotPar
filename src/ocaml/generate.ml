(* Generate scala code from an AST *)
(* Ripped off from AST reverser *)

open Ast;;

exception NotImplemented;;
let ind = "  ";;

let rec gen_expression inds expression =
  match expression with
    (* Assignment_expression(rv, lv) -> *)
    (*   (gen_expression rv) ^ "=" ^ (gen_expression lv) *)
  (* | Declaration(type_dec, expr) -> *)
  (*     "var " ^ (gen_type type_dec) ^ " " ^ (gen_expression expr) *)
  (* | Declaration_expression(type_dec, rv, lv) -> *)
  (*     (gen_type type_dec) ^ " " ^ (gen_expression rv) ^ *)
  (*     "=" ^ (gen_expression rv) *)
  (* | Array_literal(exprs) -> *)
  (*     "[" ^ (String.concat ", " (List.map gen_expression exprs)) ^ "]" *)
  (* | List_comprehension(expr, params, exprs, if_cond) -> *)
  (*     "[" ^ (gen_expression expr) ^ " for " ^ *)
  (*     (String.concat ", " (List.map gen_param params)) ^ " in " ^ *)
  (*     (String.concat ", " (List.map gen_expression exprs)) ^ *)
  (*     (match if_cond with *)
  (*       Empty_expression -> "" *)
  (*     | _ -> " if " ^ (gen_expression if_cond)) *)
  (*     ^ "]" *)
  (*       (\* unary operators *\) *)
  | Unop(op,expr) -> (gen_unop op) ^ (gen_expression inds expr)
        (* all binary operators *)
  | Binop(expr1,op,expr2) ->
      (gen_expression inds expr1) ^
      (gen_binop op) ^
      (gen_expression inds expr2)
        (* postfix *)
  (* | Function_call(expr, exprs) -> *)
  (*     (gen_expression expr) ^ "(" ^ *)
  (*     (String.concat ", " (List.map gen_expression exprs)) ^ ")" *)
  (* | Array_access(expr, expr2) -> *)
  (*     (gen_expression expr) ^ "[" ^ (gen_expression expr2) ^ "]" *)
  (*       (\* *\) *)
  | Variable(str) -> str
        (* constants *)
  | Char_literal(c) -> "'" ^ (String.make 1 c) ^ "'"
  | Number_literal(n) -> (string_of_float n)
  | String_literal(s) -> "\"" ^ s ^ "\""
  | Boolean_literal(b) -> (if (b) then "true" else "false")
  | Nil_literal -> "" (* is this legal? *)
      (* *)
  (* | Anonymous_function(type_def, params, block) -> *)
  (*     "func:" ^ (gen_type type_def) ^ "(" ^ *)
  (*     (String.concat ", " (List.map gen_param params)) ^ ")" ^ *)
  (*     "{" ^ *)
  (*     (gen_statements block) ^ "}" *)
  (* | Function_expression(state) -> *)
  (*     (gen_statement state) *)
  (*     (\* *\) *)
  (* | Empty_expression -> "" *)
  | anything -> raise NotImplemented

and gen_unop op =
  match op with
    Neg -> "-"
  | Not -> "not"
and gen_binop op =
  match op with
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

and gen_basic_type btype =
  match btype with
    Void_type -> ""
  | Number_type -> "Double"
  | Char_type -> "Char"
  | Boolean_type -> "Boolean"
and gen_type var_type =
  match var_type with
    Basic_type(b) -> (gen_basic_type b)
  (* | Array_type(a) -> (gen_type a) ^ "[]" *)
  (* | Fixed_array_type(a,expr) -> *)
  (*     (gen_type a) ^ "[" ^ (gen_expression expr) ^ "]" *)
  (* | Func_type(ret_type, param_types) -> *)
  (*     "func:" ^ (gen_type ret_type) ^ "(" ^ *)
  (*     (String.concat ", " (List.map gen_type param_types)) ^ ")" *)
  (* | Func_param_type(ret_type, params) -> *)
  (*     "func:" ^ (gen_type ret_type) ^ "(" ^ *)
  (*     (String.concat ", " (List.map gen_param params)) ^ ")" *)
  | _ -> raise NotImplemented

and gen_param inds parm =
  (* match parm with *)
  (*   Param(param_type, varname) -> *)
  (*     (gen_type param_type) ^ " " ^ (gen_expression varname) *)
  ""

and gen_selection inds select =
  (* "if(" ^ (gen_expression select.if_cond) ^ ")" ^ *)
  (*   "{" ^ (gen_statements select.if_body) ^ "}" ^ *)
  (*     (if ((List.length select.elif_conds) != 0) then *)
  (*       let gen_elif cond body = *)
  (*         "elif(" ^ (gen_expression cond) ^ ")" ^ *)
  (*         "{" ^ (gen_statements body) ^ "}" *)
  (*       in  *)
  (*       (String.concat "" *)
  (*          (List.map2 gen_elif select.elif_conds select.elif_bodies)) *)
  (*     else "") ^ *)
  (*     (if (select.else_body != []) then *)
  (*       "else {" ^ (gen_statements select.else_body) ^ "}" *)
  (*     else "") *)
  ""

and gen_statement inds stat =
  match stat with
    Expression(e) -> (gen_expression inds e) ^ ";\n"
  | Statements(s) -> (gen_statements inds s) ^ "\n" ^ inds
  (* | Selection(s) -> (gen_selection s) ^ "\n" *)
  (* | Iteration(dec,check,incr, stats) -> *)
  (*     "for(" ^ (gen_expression dec) ^ "," ^ *)
  (*     (gen_expression check) ^ "," ^ *)
  (*     (gen_expression incr) ^ ")" ^ *)
  (*     "{" ^ (gen_statements stats) ^ "}\n" *)
  (* | Jump(j) -> "return " ^ (gen_expression j) ^ ";\n" *)
  | Function_definition(name, ret_type, params, sts) ->
      (* !!! *)
      (match name with
        "main" ->
          inds ^ "def main" ^ "(" ^
          (if List.length(params)==0 then
            "args: Array[String]"
          else
            (String.concat ", " (List.map (gen_param inds) params))
          ) ^ ")" ^
          (gen_type ret_type) ^
          " {\n" ^ (gen_statements (ind ^ inds) sts) ^ inds ^ "}"
      | anything ->
          "def " ^ name ^ ":" ^ (gen_type ret_type) ^
          "(" ^ (String.concat ", " (List.map (gen_param inds) params)) ^ ")" ^
          " {" ^ (gen_statements inds sts) ^ "}"
      )
  | anything -> raise NotImplemented
   

and gen_statements inds statements =
  match statements with
    head::tail ->
      (gen_statement inds head) ^ inds ^ "\n" ^ (gen_statements inds tail)
  | _-> ""


(* and gen_import import = *)
(* (\* include files *\) *)
(*   match import with *)
(*     Import(s) -> "import " ^ s ^ ";\n" *)
(* and gen_imports imports = *)
(*   match imports with *)
(*     head::tail -> (gen_import head) ^ (gen_imports tail) *)
(*   | _-> "" *)
(* ;; *)

let gen_program program =
  match program with
    (* leave out imports for now *)
    Program(imports, statements) ->
      "object Main {\n" ^
      (* (gen_imports imp) ^ "\n" ^ *)
      (gen_statements ind statements) ^
      "}\n"
;;
