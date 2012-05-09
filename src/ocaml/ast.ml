(* Abstract syntax tree definitions *)

open Str;;
exception Error of string
module StringMap = Map.Make(String);;

type unop = Neg | Not

type binop =
    Add | Sub | Mult | Div | Mod
  | Eq | Neq | Lt | Leq | Gt | Geq
  | And | Or

type basic_type =
    Void_type
  | Number_type
  | Char_type
  | Boolean_type

and var_type =
    Basic_type of basic_type
  | Array_type of var_type
  | Fixed_array_type of var_type * expression
  | Func_type of var_type * var_type list * symbol_table ref 
  | Func_param_type of var_type * param list
  | Any_type

and symbol_table = {
  mutable table : var_type StringMap.t;
  mutable parent: symbol_table option; 
  mutable children : symbol_table list;
  mutable pure : bool;
  mutable associative : bool;
}

and expression =
    Assignment_expression of expression * expression
  | Declaration of var_type * expression
  | Declaration_expression of var_type * expression * expression
  | Array_literal of expression list
  | List_comprehension of expression * param list * expression list * 
    expression * symbol_table
        (* unary operators *)
  | Unop of unop * expression
        (* all binary operators *)
  | Binop of expression * binop * expression
        (* postfix *)
  | Function_call of expression * expression list
  | Array_access of expression * expression
  | Variable of string
  (* constants *)
  | Char_literal of char
  | Number_literal of float
  | String_literal of string
  | Boolean_literal of bool
  | Nil_literal
  | Anonymous_function of var_type * param list * statements * symbol_table
  | Function_expression of statement (* hacky, but whatever *)
  | Empty_expression

and param = Param of var_type * expression

and selection_statement = {
  if_cond : expression;
  if_body : statements;
  if_sym_tabl : symbol_table;
  else_body : statements;
  else_sym_tabl : symbol_table;
  elif_conds : expression list;
  elif_bodies : statements list;
  elif_sym_tabl : symbol_table list;
}

and statement =
    Expression of expression
  | Statements of statements (* compound statements *)
  | Selection of selection_statement
  | Iteration of expression * expression * expression * statements *
    symbol_table * symbol_table 
  | Jump of expression
  | Function_definition of string * var_type * param list * statements *
    symbol_table  

and statements = statement list

and import = Import of string

and imports = import list


and program = Program of imports * statements * symbol_table

;;

let  make_symbol_table p = 
  {
    table = StringMap.empty;
    parent = p;
    children = [];
    pure = false;
    associative = false;
  }
;;
let builtin_list = 
  [ ("cat" ,  
        Func_type( Array_type(Any_type)
        , [Array_type(Any_type) ; Array_type(Any_type) ] 
        , (ref (make_symbol_table None);))) ;
    ("each" ,  
        Func_type( Basic_type(Void_type)
        , [Array_type(Any_type) ;
        Func_type(Any_type,  [Any_type ; ], (ref (make_symbol_table None );))
        ]
        , (ref (make_symbol_table None);)));
    ("fill", 
        Func_type( Array_type(Any_type),
        [Func_type(Any_type, [Any_type], (ref (make_symbol_table None);)) ;
        Basic_type(Number_type)]
        , (ref (make_symbol_table None);))) ; 
    ("filter", 
        Func_type( Basic_type(Boolean_type) 
        , [Array_type(Any_type) ; Func_type(Array_type(Any_type) ,
        [Basic_type(Number_type) ; Basic_type(Number_type) ], (ref
        (make_symbol_table None);)) ], (ref (make_symbol_table None);)))  ;
    ("len", 
        Func_type(Basic_type(Number_type), [Array_type(Any_type)] , (ref
        (make_symbol_table None); )) );
    ("map",  
        Func_type( Array_type(Any_type), 
        [Array_type(Any_type) ; 
        Func_type(Any_type,  [Any_type] , (ref
        (make_symbol_table None);)) ] , (ref (make_symbol_table None);)) ) ;
    ("reduce",
        Func_type( Any_type,
          [Array_type(Any_type) ; 
          Func_type( Any_type, [Any_type ; Any_type] ,(ref (make_symbol_table None);)) ;
          Any_type ]
        , (ref (make_symbol_table None);) )) ;
    ("zip",
        Func_type(Array_type(Array_type(Any_type)), 
        [Array_type(Any_type) ; Array_type(Any_type)] , (ref (make_symbol_table None);)) ) ;
    ("acos",
        Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("asin",
        Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("atan",
        Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("cos",
        Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("exp",
        Func_type(Basic_type(Number_type), 
        [Basic_type(Number_type) ; Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("ln",
        Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("log",
        Func_type(Basic_type(Number_type), 
        [Basic_type(Number_type) ;  Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("sin",
        Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("sqrt",
        Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("tan",
        Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("ceil",
        Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("floor",
        Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("trunc",
        Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("round",
        Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("rand",
    Func_type(Basic_type(Number_type), [Basic_type(Number_type)], (ref (make_symbol_table None);))) ;
    ("print",
        Func_type(Basic_type(Void_type),
        [Any_type], (ref (make_symbol_table None);))) ;
    ("println",
        Func_type(Basic_type(Void_type),
        [Any_type], (ref (make_symbol_table None);))) ;
    ("printerr",
        Func_type(Basic_type(Void_type),
        [Any_type], (ref (make_symbol_table None);))) ;
    ("read",
        Func_type(Any_type,
        [], (ref (make_symbol_table None);))) ;
    ("readln",
        Func_type(Any_type,
        [], (ref (make_symbol_table None);))) 
  ]

let reserved_words = 
  [ "abstract" ; "case"    ; "catch"   ; "class"  ; "def"      ;
    "do"       ; "else"    ; "extends" ; "false"  ; "final"    ;
    "finally"  ; "for"     ; "forSome" ; "if"     ; "implicit" ;
    "import"   ; "lazy"    ; "match"   ; "new"    ; "null"     ;
    "object"   ; "override"; "package" ; "private"; "protected";
    "return"   ; "sealed"  ; "super"   ; "this"   ; "throw"    ;
    "trait"    ; "try"     ; "true"    ; "type"   ; "val"      ; 
    "var"      ; "while"   ; "with"    ; "yield"  ; "cat"      ;
    "each"     ; "fill"    ; "filter"  ; "len"    ; "map"      ;
    "reduce"   ; "zip"     ; "acos"    ; "asin"   ; "atan"     ;
    "cos"      ; "exp"     ; "ln"      ; "log"    ; "sin"      ;
    "sqrt"     ; "tan"     ; "ceil"    ; "floor"  ; "trunc"    ;
    "round"    ; "rand"    ; "print"   ; "println";
    "printerr" ; "read"    ; "readln"
  ]

;;
let add_to_symbol_table id id_type sym_table =
    if (List.mem id reserved_words) then raise (Error "Can not declare function or variable with a reserved word")
    else ignore(sym_table.table <- StringMap.add id id_type sym_table.table);
    ()
;;

let global_add id id_type sym_table =
    sym_table.table <- StringMap.add id id_type sym_table.table
;; 
let make_global_table p =
  let s_t = make_symbol_table p in
  let add_func (name,t) = global_add name t s_t in
  ignore(List.map add_func builtin_list);
  s_t
;;

    
(* reverse string out for the AST *)

(* let ind = "  ";; *)

let rec string_of_expression expression =
  match expression with
    Assignment_expression(rv, lv) ->
      (string_of_expression rv) ^ "=" ^ (string_of_expression lv)
  | Declaration(type_dec, expr) ->
      (string_of_type type_dec) ^ " " ^ (string_of_expression expr)
  | Declaration_expression(type_dec, rv, lv) ->
      (string_of_type type_dec) ^ " " ^ (string_of_expression rv) ^
      "=" ^ (string_of_expression lv)
  | Array_literal(exprs) ->
      "[" ^ (String.concat ", " (List.map string_of_expression exprs)) ^ "]"
  | List_comprehension(expr, params, exprs, if_cond, s) ->
      "[" ^ (string_of_expression expr) ^ " for " ^
      (String.concat ", " (List.map string_of_param params)) ^ " in " ^
      (String.concat ", " (List.map string_of_expression exprs)) ^
      (match if_cond with
        Empty_expression -> ""
      | _ -> " if " ^ (string_of_expression if_cond))
      ^ "]"
        (* unary operators *)
  | Unop(op,expr) -> (string_of_unop op) ^ (string_of_expression expr)
        (* all binary operators *)
  | Binop(expr1,op,expr2) ->
      (string_of_expression expr1) ^
      (string_of_binop op) ^
      (string_of_expression expr2)
        (* postfix *)
  | Function_call(expr, exprs) ->
      (string_of_expression expr) ^ "(" ^
      (String.concat ", " (List.map string_of_expression exprs)) ^ ")"
  | Array_access(expr, expr2) ->
      (string_of_expression expr) ^ "[" ^ (string_of_expression expr2) ^ "]"
        (* *)
  | Variable(str) -> str
        (* constants *)
  | Char_literal(c) -> "'" ^ (String.make 1 c) ^ "'"
  | Number_literal(n) -> (string_of_float n)
  | String_literal(s) -> "\"" ^ s ^ "\""
  | Boolean_literal(b) -> (if (b) then "true" else "false")
  | Nil_literal -> "nil" (* is this legal? *)
      (* *)
  | Anonymous_function(type_def, params, block, sym_tabl) ->
      "func:" ^ (string_of_type type_def) ^ "(" ^
      (String.concat ", " (List.map string_of_param params)) ^ ")" ^
      "{" ^
      (string_of_statements block) ^ "}"
  | Function_expression(state) ->
      (string_of_statement state)
      (* *)
  | Empty_expression -> ""

and string_of_unop op =
  match op with
    Neg -> "-"
  | Not -> "not"
and string_of_binop op =
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

and string_of_basic_type btype =
  match btype with
    Void_type -> "void"
  | Number_type -> "number"
  | Char_type -> "char"
  | Boolean_type -> "bool"
and string_of_type var_type =
  match var_type with
    Basic_type(b) -> (string_of_basic_type b)
  | Array_type(a) -> (string_of_type a) ^ "[]"
  | Fixed_array_type(a,expr) ->
      (string_of_type a) ^ "[" ^ (string_of_expression expr) ^ "]"
  | Func_type(ret_type, param_types, sym_ref) ->
      "func:" ^ (string_of_type ret_type) ^ "(" ^
      (String.concat ", " (List.map string_of_type param_types)) ^ ")"
  | Func_param_type(ret_type, params) ->
      "func:" ^ (string_of_type ret_type) ^ "(" ^
      (String.concat ", " (List.map string_of_param params)) ^ ")"
  | Any_type -> "Any_type"

and string_of_param parm =
  match parm with
    Param(param_type, varname) ->
      (string_of_type param_type) ^ " " ^ (string_of_expression varname)

and string_of_selection select =
  "if(" ^ (string_of_expression select.if_cond) ^ ")" ^
    "{" ^ (string_of_statements select.if_body) ^ "}" ^
      (if ((List.length select.elif_conds) != 0) then
        let gen_elif cond body =
          "elif(" ^ (string_of_expression cond) ^ ")" ^
          "{" ^ (string_of_statements body) ^ "}"
        in 
        (String.concat ""
           (List.map2 gen_elif select.elif_conds select.elif_bodies))
      else "") ^
      (if (select.else_body != []) then
        "else {" ^ (string_of_statements select.else_body) ^ "}"
      else "")

and string_of_statement stat =
  match stat with
    Expression(e) -> (string_of_expression e) ^ ";\n"
  | Statements(s) -> (string_of_statements s) ^ "\n"
  | Selection(s) -> (string_of_selection s) ^ "\n"
  | Iteration(dec,check,incr, stats, sym_tabl, head_sym_tabl) ->
      "for(" ^ (string_of_expression dec) ^ "," ^
      (string_of_expression check) ^ "," ^
      (string_of_expression incr) ^ ")" ^
      "{" ^ (string_of_statements stats) ^ "}\n"
  | Jump(j) -> "return " ^ (string_of_expression j) ^ ";\n"
  | Function_definition(name, ret_type, params, sts, sym_tabl) ->
      "func " ^ name ^ ":" ^ (string_of_type ret_type) ^
      "(" ^ (String.concat ", " (List.map string_of_param params)) ^ ")" ^
      "{\n" ^ (string_of_statements sts) ^ "}\n"
and string_of_statements statements =
  match statements with
    head::tail -> (string_of_statement head) ^"\n"^ (string_of_statements tail)
  | _-> ""

and string_of_import import =
  match import with
    Import(s) -> "import " ^ s ^ ";\n"
and string_of_imports imports =
  match imports with
    head::tail -> (string_of_import head) ^ (string_of_imports tail)
  | _-> ""
;;
let string_of_program program =
  match program with
    Program(imp, stat, symbol_table) ->
      (string_of_imports imp) ^ "\n" ^ (string_of_statements stat)
;;


(************************************************************)
(* print out a non-sissy ast repr *)

let gind = "    ";;

let rec repr0 name =
  "<" ^ name ^ ">"
and repr1 ind name str1 =
  "<" ^ name ^ "\n" ^ ind ^ str1 ^ ">"
and repr2 ind name str1 str2 =
  repr1 ind name (String.concat ("\n" ^ ind) [str1; str2])
and repr3 ind name str1 str2 str3 =
  repr1 ind name (String.concat ("\n" ^ ind) [str1; str2; str3])
and repr4 ind name str1 str2 str3 str4 =
  repr1 ind name (String.concat ("\n" ^ ind) [str1; str2; str3; str4])
and repr5 ind name str1 str2 str3 str4 str5 =
  repr1 ind name (String.concat ("\n" ^ ind) [str1; str2; str3; str4; str5])
;;

let repr_list ind fn list =
  "[" ^ (String.concat ("\n" ^ ind) (List.map fn list)) ^ "]"
;;

let rec repr_table ind table =
  let repr_entry key data folded =
    folded ^ "\n" ^ ind ^ key ^ ": " ^ (repr_of_type ind data)
  in
  "[Table" ^
  (StringMap.fold repr_entry table.table "")
  ^ "]"

(* the actual ast repr *)
and repr_of_expression ind expression =
  let ind = ind ^ gind in
  match expression with
    Assignment_expression(rv, lv) ->
      (repr2 ind "Assign"
         (repr_of_expression ind rv) (repr_of_expression ind lv))
  | Declaration(type_dec, expr) ->
      (repr2 ind "Declare"
         (repr_of_type ind type_dec) (repr_of_expression ind expr))
  | Declaration_expression(type_dec, rv, lv) ->
      (repr3 ind "Declare+Assign"
         (repr_of_type ind type_dec)
         (repr_of_expression ind rv)
         (repr_of_expression ind lv))
  | Array_literal(exprs) ->
      (repr1 ind "Array Literal"
         (repr_of_expressions ind exprs))
  | List_comprehension(expr, params, exprs, if_cond, table) ->
      (repr5 ind "ListComperehension"
         (repr_table ind table)
         (repr_of_expression ind expr)
         (repr_of_params ind params)
         (repr_of_expressions ind exprs)
         (repr_of_expression ind if_cond))
        (* unary operators *)
  | Unop(op,expr) ->
      (repr2 ind "UnaryOperation"
         (repr_of_unop ind op) (repr_of_expression ind expr))
        (* all binary operators *)
  | Binop(expr1,op,expr2) ->
      (repr3 ind "BinaryOperation"
         (repr_of_expression ind expr1)
         (repr_of_binop ind op)
         (repr_of_expression ind expr2))
        (* postfix *)
  | Function_call(expr, exprs) ->
      (repr2 ind "FunctionCall"
         (repr_of_expression ind expr)
         (repr_of_expressions ind exprs))
  | Array_access(expr, expr2) ->
      (repr2 ind "ArrayAccess"
         (repr_of_expression ind expr)
         (repr_of_expression ind expr2))
        (* *)
  | Variable(str) -> (repr1 ind "Variable" str)
        (* constants *)
  | Char_literal(c) -> (repr1 ind "CharLiteral" (String.make 1 c))
  | Number_literal(n) -> (repr1 ind "NumberLiteral" (string_of_float n))
  | String_literal(s) -> (repr1 ind "StringLiteral" s)
  | Boolean_literal(b) ->
      (repr1 ind "BooleanLiteral"
         (if (b) then "true" else "false"))
  | Nil_literal -> repr0 "NilLiteral"
      (* *)
  | Anonymous_function(type_def, params, block, table) ->
      (repr4 ind "AnonymousFunction"
         (repr_table ind table)
         (repr_of_type ind type_def)
         (repr_of_params ind params)
         (repr_of_statements ind block))
  | Function_expression(state) ->
      (repr1 ind "NamedFunction"
         (repr_of_statement ind state))
      (* *)
  | Empty_expression -> repr0 "EmptyExpression"
and repr_of_expressions ind exprs =
  let ind = ind ^ gind in
  (repr_list ind (repr_of_expression ind) exprs)

and repr_of_unop ind op =
  match op with
    Neg -> "Negative"
  | Not -> "Not"
and repr_of_binop ind op =
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

and repr_of_basic_type ind btype =
  match btype with
    Void_type -> repr0 "VoidType"
  | Number_type -> repr0 "NumberType"
  | Char_type -> repr0 "CharType"
  | Boolean_type -> repr0 "BooleanType"
and repr_of_type ind var_type =
  let ind = ind ^ gind in
  match var_type with
    Basic_type(b) -> repr1 ind "BasicType" (repr_of_basic_type ind b)
  | Array_type(a) -> repr1 ind "ArrayType" (repr_of_type ind a)
  | Fixed_array_type(a,expr) ->
      repr2 ind "ArrayTypeFixed"
        (repr_of_type ind a)
        (repr_of_expression ind expr)
  | Func_type(ret_type, param_types, sym_ref) ->
      repr2 ind "FunctionType"
        (repr_of_type ind ret_type)
        (repr_of_types ind param_types)
  | Func_param_type(ret_type, params) ->
      repr2 ind "FunctionType+Param"
        (repr_of_type ind ret_type)
        (repr_of_params ind params)
  | Any_type -> "Any_type"

and repr_of_types ind types =
  repr_list ind (repr_of_type ind) types

and repr_of_param ind parm =
  let ind = ind ^ gind in
  match parm with
    Param(param_type, varname) ->
      (repr2 ind "Parameter"
         (repr_of_type ind param_type) (repr_of_expression ind varname))
and repr_of_params ind params =
  repr_list ind (repr_of_param ind) params

    (* !!! add function table printing *)
and repr_of_selection ind select =
  let ind = ind ^ gind in
  (repr3 ind "Selection(if)"
     (let ind = ind ^ gind in
     (repr3 ind "If"
        (repr_table ind select.if_sym_tabl)
        (repr_of_expression ind select.if_cond)
        (repr_of_statements ind select.if_body)))
     (* maybe print these out in pairs? *)
     (let ind = ind ^ gind in
     (repr3 ind "Elifs"
        (repr_list ind (repr_table ind) select.elif_sym_tabl)
        (repr_of_expressions ind select.elif_conds)
        (repr_list ind (repr_of_statements ind) select.elif_bodies)))
     (let ind = ind ^ gind in
     (repr2 ind "Else"
        (repr_table ind select.else_sym_tabl)
        (repr_of_statements ind select.else_body))))

and repr_of_statement ind stat =
  let ind = ind ^ gind in
  match stat with
    Expression(e) ->
      (repr1 ind "Expression" (repr_of_expression ind e)) ^ "\n" ^ ind
  | Statements(s) ->
      (repr1 ind "Statements" (repr_of_statements ind s)) ^ "\n" ^ ind
  | Selection(s) ->
      (repr1 ind "If" (repr_of_selection ind s)) ^ "\n" ^ ind
  | Iteration(dec,check,incr, stats, table, header_symbol_Table) ->
      (repr5 ind "For"
         (repr_table ind table)
         (repr_of_expression ind dec)
         (repr_of_expression ind check)
         (repr_of_expression ind incr)
         (repr_of_statements ind stats))
  | Jump(j) ->
      (repr1 ind "Return" (repr_of_expression ind j)) ^ "\n" ^ ind
  | Function_definition(name, ret_type, params, sts, table) ->
      (repr5 ind "Function_Definition"
         (repr_table ind table)
         name
         (repr_of_type ind ret_type)
         (repr_of_params ind params)
         (repr_of_statements ind sts))
and repr_of_statements ind statements =
  repr_list ind (repr_of_statement ind) statements

and repr_of_import ind import =
  (let ind = ind ^ gind in
  match import with
    Import(s) -> repr1 ind "Import " s)
and repr_of_imports ind imports =
  repr_list ind (repr_of_import ind) imports
;;

let repr_of_program program =
  match program with
    Program(imp, stat, symbol_table) ->
      (repr2 gind "Program"
         (repr_of_imports gind imp) (repr_of_statements gind stat)) ^ "\n"
;;
