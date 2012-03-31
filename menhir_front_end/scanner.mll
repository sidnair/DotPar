{
  open Parser
  (*exception Error of string*)
}

let newline    = '\n' | "\r\n"
let whitespace = [' ' '\t'] | newline
let digit      = ['0'-'9']
let number     = digit+ | digit* '.' digit+
let alpha      = ['_' 'a'-'z' 'A'-'Z']
let alphanum   = alpha | digit
let identifier = alpha alphanum*

rule token = parse
| whitespace          { token lexbuf }
(*| "/*"              { multicomment lexbuf }*)
| "//"                { single_comment lexbuf }
| "import"            { IMPORT }
| "true"              { TRUE }
| "false"             { FALSE }
| "nil"               { NIL }
| "boolean"           { BOOLEAN }
| "char"              { CHAR }
| "func"              { FUNC }
| "number"            { NUMBER }
| "void"              { VOID }
| "if"                { IF }
| "else"              { ELSE }
| "elif"              { ELIF }
| "for"               { FOR }
| "in"                { IN }
| "return"            { RETURN }

| '('                 { LPAREN }
| ')'                 { RPAREN }
| '{'                 { LBRACE }
| '}'                 { RBRACE }
| '['                 { LBRACK }
| ']'                 { RBRACK }
| ';'                 { SEMI }
| ','                 { COMMA }
| ':'                 { COLON }

| "&&"                { AND }
| "||"                { OR }
| "!"                 { NOT }
| ">"                 { GT }
| "<"                 { LT }
| ">="                { GEQ }
| "<="                { LEQ }
| "=="                { EQ }
| "!="                { NEQ }
| "="                 { ASSIGN }
| "+"                 { ADD }
| "-"                 { SUB }
| "*"                 { MULT }
| "/"                 { DIV }
| "%"                 { REM }
| number              { NUM_LITERAL }
| identifier          { IDENTIFIER }
| "'(\\.|[^'\n])'"    { CHAR_LITERAL; }
(*| "\"(\\.|[^\\"\n])*\"" { STRING_LITERAL; }*)
| _ as char           { raise (Failure("illegal character " ^ Char.escaped char)) }


and single_comment = parse
  | ['\n' '\r']      { token lexbuf }
  | _                { single_comment lexbuf }
