type token =
  | IMPORT
  | TRUE
  | FALSE
  | NIL
  | BOOLEAN
  | CHAR
  | FUNC
  | NUMBER
  | VOID
  | IF
  | ELSE
  | ELIF
  | FOR
  | IN
  | RETURN
  | SEMI
  | COMMA
  | COLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | AND
  | OR
  | NOT
  | GT
  | LT
  | GEQ
  | LEQ
  | EQ
  | NEQ
  | ASSIGN
  | ADD
  | SUB
  | MULT
  | DIV
  | REM
  | STRING_LITERAL
  | NUM_LITERAL
  | CHAR_LITERAL
  | IDENTIFIER

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
