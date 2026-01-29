open Liblambda.Toplevel

let rec prompt env lexbuf =
  print_string "# "; flush stdout;
  let env, reduced_term = eval env (read lexbuf) in
  let () = alpha_find env (print reduced_term) in
  prompt env lexbuf

let _ =
  let lexbuf = Lexing.from_channel stdin in
  prompt default_env lexbuf
