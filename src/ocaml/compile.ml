open Ast;;
open Semantic;;
open Parallelizer;;

let rec ast_generate stream debug =
  let ast = parse_ast stream in
  (* Perform semantic transformations, import preprocessing, and semantic
   * analysis. The code generation to Scala happens elsewhere. *)
  let ast = Transform.reverse_tree ast in
  let ast = insert_imports_program ast debug in
  ignore(Semantic.generate_sast ast debug);
  let ast = Parallelizer.parallelize ast in
  ast

and parse_ast stream =
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
  ast

(* TODO: import only once *)
and insert_import import debug_switch =
  match import with
    Import(s) ->
      let filename = s ^ ".par" in
      let file = open_in filename in
      (ast_generate file debug_switch)
and insert_imports imports debug_switch =
  let programs =
    List.map (fun x -> (insert_import x debug_switch)) imports in
  let get_statements program =
    match program with
    | Program(imports, statements, symbol_table) -> statements
  in
  let statements = List.map get_statements programs in
  let join a b = a @ b in
  List.fold_left join [] statements
and insert_imports_program program debug_switch =
  match program with
    Program(imports, statements, symbol_table) ->
      let import_statements = (insert_imports imports debug_switch) in
      (Program ([], (import_statements @ statements), symbol_table))
