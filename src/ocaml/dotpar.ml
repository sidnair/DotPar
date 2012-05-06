open Compile;;

(* utility function *)
let debug_state = true
let debug str =
  if debug_state then print_string (str) else ()

let _ =
  let generate self_switch ast_switch gen_switch =
    let ast = Compile.ast_generate stdin in
    let prog_str = Generate.gen_program ast in
    (if self_switch then
      Printf.printf "%s" (Ast.string_of_program ast));
    (if ast_switch then
      Printf.printf "%s" (Ast.repr_of_program ast));
    (if gen_switch or not (ast_switch or self_switch) then
      Printf.printf "%s" prog_str)
  in
  (* check if there are any switches *)
  let args = Array.to_list
      (Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1)) in
  let switch check switches =
    (try
      List.find (fun x -> x = check) switches;
      true
    with Not_found -> false) in
  (* let debug = switch "--debug" args in *)
  let self_switch = switch "--dotpar" args in
  let ast_switch = switch "--ast" args in
  let gen_switch = switch "--scala" args in
  let parser_switch = switch "--parser-only" args in
  (if parser_switch then
    ignore(parse_ast stdin)
  else
    generate self_switch ast_switch gen_switch)
