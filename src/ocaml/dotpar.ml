open Compile;;

let ast = ast_generate stdin in
Printf.printf "%s" (Generate.gen_program ast);;
