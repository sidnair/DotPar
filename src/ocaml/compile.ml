open Ast;;
open Semantic;;

let rec ast_generate stream =
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
  (* Perform semantic transformations, import preprocessing, and semantic
   * analysis. The code generation to Scala happens separately. *)
  let ast = Transform.reverse_tree ast in
  let ast = insert_imports_program ast in
  ignore( Semantic.generate_sast ast );
  ast

(* TODO: import only once *)
and insert_import import =
  match import with
    Import(s) ->
      let filename = s ^ ".par" in
      let file = open_in filename in
      (ast_generate file)
and insert_imports imports =
  let programs =
    List.map (fun x -> (insert_import x)) imports in
  let get_statements program =
    match program with
    | Program(imports, statements, symbol_table) -> statements in
  let statements = List.map get_statements programs in
  let join a b = a @ b in
  List.fold_left @ [] statements
and insert_imports_program program =
  match program with
    Program(imports, statements, symbol_table) ->
      let import_statements = (insert_imports imports) in
      (Program ([], (import_statements @ statements), symbol_table))
