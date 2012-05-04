open Ast;;
open Semantic;;

let rec ast_generate stream self_switch ast_switch =
  let lexbuf = Lexing.from_channel stream in 
  let ast =
    try
      Parser.program Scanner.token lexbuf
    with
    | Failure(s)             -> prerr_endline ("Error"); exit 1
    | Parsing.Parse_error    ->
      let p = Lexing.lexeme_start_p lexbuf in
      let l = p.Lexing.pos_lnum in
      let c = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
      Printf.fprintf stderr "Parsing error at line %d, char %d\n." l c;
      exit 1
  in
  (* Semantic transforms *)
  let ast = Transform.reverse_tree ast in

  (* do import preprocessing *)
  let ast = insert_imports_program ast self_switch ast_switch in
  
  (* Semantic analysis *)
  ignore( Semantic.generate_sast ast );
  (* DEBUG: print the AST *)
  (if self_switch then
    Printf.printf "%s" (Ast.string_of_program ast));
  (if ast_switch then
    Printf.printf "%s" (Ast.repr_of_program ast));
  ast
  (* Code generation comes after *)

 (*import ------------------------------------------------------------  *)
and insert_import import self_switch ast_switch =
 (*include files *)
 (*!!! TODO: import only once *)
  match import with
    Import(s) ->
      let filename = s ^ ".par" in
      let file = open_in filename in
      (ast_generate file self_switch ast_switch)
and insert_imports imports self_switch ast_switch =
  let programs =
    List.map (fun x -> (insert_import x self_switch ast_switch)) imports in
  let get_statements program = 
    match program with
      Program(imports, statements, symbol_table) ->
        statements in
  let statements = List.map get_statements programs in
  let join a b = a @ b in
  List.fold_left join [] statements
and insert_imports_program program self_switch ast_switch =
  match program with
    Program(imports, statements, symbol_table) ->
      let import_statements = (insert_imports imports self_switch ast_switch) in
      (Program ([], (import_statements @ statements), symbol_table))
;;
