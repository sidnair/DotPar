let _ =
  try
    let lexbuf = Lexing.from_channel stdin in 
      Parser.program Scanner.token lexbuf
  with
    | Failure(s)             -> prerr_endline ("Error"); exit 1
    | Parsing.Parse_error    -> prerr_endline ("Syntax error"); exit 1
