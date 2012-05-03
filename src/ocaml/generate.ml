(* Generate scala code from an AST *)
(* Ripped off from AST reverser *)

open Ast;;
open Printf;;

exception NotImplemented;;
exception SemanticError;;
let ind = "  ";;

let rec gen_expression inds expression =
  let next_inds = (ind ^ inds) in
  match expression with
    Assignment_expression(lv, rv) ->
      (gen_expression inds lv) ^ " = " ^ (gen_expression inds rv)
  | Declaration(type_dec, expr) ->
      "var " ^ (gen_expression inds expr) ^ ":" ^ (gen_type type_dec) ^
      " = " ^ (gen_expression inds (gen_initial type_dec))
  | Declaration_expression(type_dec, lv, rv) ->
      "var " ^ (gen_expression inds lv) ^ ":" ^ (gen_type type_dec) ^
      " = " ^ (gen_expression inds rv)
  | Array_literal(exprs) ->
      "Array(" ^ (String.concat ", "
                    (List.map (gen_expression inds) exprs)) ^ ")"
  | List_comprehension(expr, params, exprs, if_cond, s) ->
      (if (List.length params) == 1 then
        let if_cond_str = (gen_expression inds if_cond) in
        "(" ^ (gen_expression inds (List.nth exprs 0)) ^
        (if (String.length if_cond_str) > 0 then
          ".filter({(" ^ (gen_param inds (List.nth params 0)) ^
          ") => " ^ if_cond_str ^ "})"
        else
          "") ^
        ".map({(" ^ (gen_param inds (List.nth params 0)) ^ 
        ") => " ^ if_cond_str ^ "})" ^ ")"
      else
        let if_cond_str = (gen_expression inds if_cond) in
        "(" ^ (gen_expression inds (List.nth exprs 0)) ^
        (if (String.length if_cond_str) > 0 then
          ".zipped.filter({(" ^
          (String.concat "," (List.map (gen_param inds) params)) ^
          ") => " ^ if_cond_str ^ "})"
        else
          "") ^
        ".zipped.map({(" ^
        (String.concat "," (List.map (gen_param inds) params)) ^ 
        ") => " ^ if_cond_str ^ "})" ^ ")"
      )
        (* unary operators *)
  | Unop(op,expr) -> (gen_unop op) ^ (gen_expression inds expr)
        (* all binary operators *)
  | Binop(expr1,op,expr2) ->
      "(" ^ (gen_expression inds expr1) ^ " " ^
      (gen_binop op) ^ " " ^
      (gen_expression inds expr2) ^ ")"
        (* postfix *)
  | Function_call(expr, exprs) ->
      (* match special functions *)
      (* !!! *)
      (match((gen_expression inds expr)) with
      | "println" -> "Dotpar.dp_println(" ^
          (String.concat ", " (List.map (gen_expression inds) exprs)) ^
          ")\n"
      | "each" -> "Dotpar.dp_each(" ^
          (String.concat ", " (List.map (gen_expression inds) exprs)) ^
          ")\n"
      | "filter" -> "Dotpar.dp_filter(" ^
          (String.concat ", " (List.map (gen_expression inds) exprs)) ^
          ")\n"
      | "map" -> "Dotpar.dp_map(" ^
          (String.concat ", " (List.map (gen_expression inds) exprs)) ^
          ")\n"
      | "reduce" -> "Dotpar.dp_reduce(" ^
          (String.concat ", " (List.map (gen_expression inds) exprs)) ^
          ")\n"
      | _ -> (gen_expression inds expr) ^ "(" ^
          (String.concat ", " (List.map (gen_expression inds) exprs)) ^ ")"
      )
  | Array_access(expr, expr2) ->
      (* uses a function-call index syntax *)
      (gen_expression inds expr) ^
      "(Dotpar.dp_array_index(" ^ (gen_expression inds expr2) ^ "))"
        (* --- *)
  | Variable(str) -> str
        (* constants *)
  | Char_literal(c) -> "'" ^ (String.make 1 c) ^ "'"
  | Number_literal(n) -> (string_of_float n)
  | String_literal(s) ->
      let char_wrap c = 
        "\'" ^ (String.make 1 c) ^ "\'"
      in
      let explode s =
        let rec exp i l =
          if (i < 0) then l else (s.[i] :: (exp (i - 1) l)) in
        List.rev (exp ((String.length s) - 1) [])
      in
      "Array(" ^ (String.concat ", " (List.map char_wrap (explode s))) ^ ")"
  | Boolean_literal(b) -> (if (b) then "true" else "false")
  | Nil_literal -> "" (* ??? is this legal? *)
        (* --- *)
  | Anonymous_function(ret_type, params, block, sym_tab) ->
      "(new Function" ^ (string_of_int (List.length params)) ^
      (* ??? *)
      (let extract_type param = 
        match param with
        | Param(param_type, varname) -> param_type
      in
      let type_list = (List.map extract_type params) @ [ret_type] in
      let fn_type = (String.concat ", " (List.map gen_type type_list)) in
      "[" ^ fn_type ^ "]") ^
      "{\n" ^ inds ^
      "def apply(" ^
      (String.concat ", " (List.map (gen_param inds) params)) ^
      ")" ^ 
      (let type_str = (gen_type ret_type) in
      if (String.length type_str) > 0 then ":" ^ type_str
      else "") ^
      " = {\n" ^ inds ^
      (gen_statements next_inds block) ^
      inds ^ "}" ^
      inds ^ "})"
  | Function_expression(state) ->
      (gen_statement inds state)
  | Empty_expression -> ""

and gen_unop op =
  match op with
    Neg -> "-"
  | Not -> "!"
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
    Void_type -> "Unit"
  | Number_type -> "Double"
  | Char_type -> "Char"
  | Boolean_type -> "Boolean"
and gen_type var_type =
  match var_type with
    Basic_type(b) -> (gen_basic_type b)
  | Array_type(a) -> "Array[" ^ (gen_type a) ^ "]"
  (* | Fixed_array_type(a,expr) -> *)
  (*     (gen_type a) ^ "[" ^ (gen_expression expr) ^ "]" *)
  | Func_type(ret_type, param_types) ->
      "((" ^ (String.concat ", " (List.map gen_type param_types)) ^ ") => " ^
      (gen_type ret_type) ^ ")"
  | Func_param_type(ret_type, params) ->
      let extract_type param = 
        match param with
        | Param(param_type, varname) -> param_type
      in
      let type_list = (List.map extract_type params) in
      "((" ^ (String.concat ", " (List.map gen_type type_list)) ^ ") => " ^
      (gen_type ret_type) ^ ")"
  | _ -> raise NotImplemented

(* this generates appropriate initial values for declarations *)
and gen_initial_basic btype =
  match btype with
    Void_type -> Empty_expression
  | Number_type -> Number_literal 0.0
  | Char_type -> Char_literal '\000'
  | Boolean_type -> Boolean_literal true
and gen_initial type_dec =
  match type_dec with
  | Basic_type(b) -> (gen_initial_basic b)
  | Array_type(a) -> Array_literal [(gen_initial a)]
  (* | Fixed_array_type(a,expr) -> *)
  (*     (gen_type a) ^ "[" ^ (gen_expression expr) ^ "]" *)
  | Func_type(ret_type, param_types) ->
      (let rec range l u =
        if ( l > u )
        then []
        else l::( range ( l + 1 ) u )
      in
      let make_param ptype name = Param(ptype, name) in
      let make_var var = Variable ("param" ^ var) in
      let params = (List.map2 make_param
                      param_types
                      (List.map make_var
                         (List.map string_of_int
                            (range 1 (List.length param_types)))))
      in
      Anonymous_function (ret_type, params,
                          [Expression (gen_initial ret_type)],
                          make_symbol_table;))
  | Func_param_type(ret_type, params) ->
      Anonymous_function (ret_type, params,
                          [Expression (gen_initial ret_type)],
                          make_symbol_table;)
  | _ -> raise NotImplemented

and gen_param inds parm =
  match parm with
    Param(param_type, varname) ->
      (gen_expression inds varname) ^ ":" ^ (gen_type param_type)

and gen_selection inds select =
  let next_inds = (ind ^ inds) in
  inds ^ "if(" ^ (gen_expression inds select.if_cond) ^ ")" ^
  "{\n" ^ next_inds ^
  (gen_statements next_inds select.if_body) ^
  inds ^ "}" ^
  (if ((List.length select.elif_conds) != 0) then
    let gen_elif cond body =
      " else if(" ^ (gen_expression next_inds cond) ^ ")" ^
      "{\n" ^ next_inds ^
      (gen_statements next_inds body) ^
      inds ^ "}"
    in
    (String.concat ""
       (List.map2 gen_elif select.elif_conds select.elif_bodies))
  else "") ^
  (if (select.else_body != []) then
    " else {\n" ^ next_inds ^
    (gen_statements next_inds select.else_body) ^
    inds ^ "}"
  else "")

and gen_statement inds stat =
  let next_inds = ind ^ inds in
  match stat with
    Expression(e) -> inds ^ (gen_expression inds e) ^ ";\n"
  | Statements(s) -> (gen_statements inds s) ^ "\n" ^ inds
  | Selection(s) -> (gen_selection inds s) ^ "\n"
  | Iteration(dec,check,incr, stats, sym_tab) ->
      inds ^ (gen_expression inds dec) ^ "\n" ^
      inds ^ "while(" ^ (gen_expression inds check) ^ ") {\n" ^
      next_inds ^ (gen_statements next_inds stats) ^
      next_inds ^ (gen_expression next_inds incr) ^
      inds ^ "}\n"
  | Jump(j) -> inds ^ "return " ^ (gen_expression inds j) ^ "\n"
  | Function_definition(name, ret_type, params, sts, sym_tab) ->
      (match name with
        "main" ->
          inds ^ "def main" ^ "(" ^
          (if List.length(params)==0 then
            "args: Array[String]" (* only place with Arrays !!! *)
          else
            (String.concat ", " (List.map (gen_param inds) params))
          ) ^ ")" ^ (* (gen_type ret_type) ^ !!! *)
          " {\n" ^ (gen_statements next_inds sts) ^ inds ^ "}"
      | anything ->
          let ret_type = (gen_type ret_type) in
          inds ^ "def " ^ name ^
          "(" ^ (String.concat ", " (List.map (gen_param inds) params)) ^ ")" ^
          (if (String.length ret_type) > 0 then ":" ^ ret_type ^ " ="
          else "") ^
          " {\n" ^ next_inds ^
          (gen_statements next_inds sts) ^
          inds ^ "}"
      )

and gen_statements inds statements =
  match statements with
    head::tail ->
      (gen_statement inds head) ^ inds ^ "\n" ^ (gen_statements inds tail)
  | _-> ""

let gen_program program =
  match program with
    Program(imports, statements) ->
      (Printf.sprintf "
import scala.collection.mutable.ArraySeq

object Dotpar {
  // multiplexed print
  def dp_println(x: Any) = {
    def dotpar_string(y: Any):String = {
      y match {
        case str: Array[Char] =>
          str.map(_.toString).reduceLeft(_+_)
        case arr: Array[_] =>
          \"[\" + arr.map(dotpar_string(_)).reduceLeft(_+\", \"+_) + \"]\"
        case _ => y.toString
      }
    }
    println(dotpar_string(x))
  }

  // converts double to int for array index, throws error
  def dp_array_index(ind: Double):Int = {
    val err = 0.00001; // chosen by fiat
    val r:Int = scala.math.round(ind).intValue
    if(scala.math.abs(r - ind) > err) {
      throw new IllegalArgumentException(\"Only integeral array indicies allowed\")
    }
    r
  }

  // each
  def dp_each[T](arr:Array[T], function:((T) => Any)) = {
    arr foreach function
  }
  // filter
  def dp_filter[T](arr:Array[T], function:((T) => Boolean)):Array[T] = {
    arr.filter(function)
  }
  // map
  def dp_map[T, TT](arr:Array[T], function:(T => TT))
      (implicit m:ClassManifest[TT]):Array[TT] = {
    (arr map function).toArray
  }
  // reduce
  def dp_reduce[T](arr:Array[T], function:((T, T) => T), start:T):T = {
    if (arr.length == 0) {
      start
    } else {
      arr reduceLeft function
    }
  }
}

object Main {
%s
}
"
    (gen_statements (ind ^ ind) statements))
;;
