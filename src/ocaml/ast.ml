(* Abstract syntax tree definitions *)

(* type boolean_literal = *)
(*     Bool of bool *)

type constant =
    (* Char_literal of char_literal *)
  (* | *) Number_literal of float
  (* | String_literal of string_literal *)
  (* | Boolean_literal of boolean_literal *)
  (* | Nil { } *)

(* type relational_expression = *)
(*     Relational_expression of string * expression * expression *)

type expression =
    (* Assignment_expression of assignment_expression *)
  (* | Array_expression of array_expression *)
  (* | Conditional_expression of conditional_expression *)
  (* | Relational_expression of relational_expression  *)
  (* | Arithmetic_expression of arithmetic_expression *)
  (* | Unary_expression of unary_expression *)
  (* | Postfix_expression of postfix_expression *)
  (* | Variable of variable *)
  (* | *) Constant of constant

(* type Selection = *)
(* type Iteration = *)
(* type Jump = *)

type basic_type =
    Void_type

type var_type =
    Basic_type of basic_type
  (* | compound_type *)

type function_definition = {
   name : string;
   ret_type : var_type;
   (* param_list : ; *)
   body : statements;
}

and statement =
    Expression of expression
  | Statements of statements (* compound statement *)
  (* | Selection of  *)
  (* | Iteration of  *)
  (* | Jump of  *)
  | Function_definition of function_definition

and statements =
    Nil
  | :: of statement * statements;;

type program = statements;;
