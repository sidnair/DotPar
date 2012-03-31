let _ =
  try
    let lexbuf = Lexing.from_channel stdin in 
    let program = Parser.program Scanner.token lexbuf in
    print_int program ; print_endline ""
  with
    | Failure(s)             -> prerr_endline ("Error"); exit 1
    | Parsing.Parse_error    -> prerr_endline ("Syntax error"); exit 1
