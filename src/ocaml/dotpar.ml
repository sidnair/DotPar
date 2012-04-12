let _ =
  let lexbuf = Lexing.from_channel stdin in 
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
  let ast = Transform.reverse_tree ast
  in
  (* DEBUG: reprint the AST *)
  Printf.printf "%s" (Ast.string_of_program ast);
  (* Semantic analysis *)
  
  (* Code generation *)
  Printf.printf "%s" (Generate.gen_program ast);
;;
