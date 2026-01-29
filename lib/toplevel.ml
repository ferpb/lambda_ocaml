open Term

(* Read toplevel expression from standard input *)
let read buf =
  Parser.main Lexer.token buf

(* Read lambda term from a string *)
let read_term str =
  Parser.lambda_term Lexer.token (Lexing.from_string str)

(* Find and print term in 'env' that are alpha equivalente to 'term' *)
let alpha_find env term =
  let matching = List.filter (fun (_, def) -> alpha_equivalent term def) env in
  let (names, _) = List.split matching in
  if List.length names > 0 then print_endline ("α > " ^ (String.concat ", " names))

(* Return the term associated to a name (or a variable if the name is not in 'env') *)
let delta env name =
  try
    let t = List.assoc name env in
    (* print_endline ("δ > expanded '" ^ name ^ "' into '" ^ (term_to_str t) ^ "'"); *)
    t
  with Not_found -> Var name

(* Subsitute 'env' definitions in 'term' *)
let rec delta_reduction env term =
  match term with
  | Term.Var atom -> delta env atom
  | Term.Abs (binder, term) -> Term.Abs (binder, (delta_reduction env term))
  | Term.App (term1, term2) -> Term.App ((delta_reduction env term1), (delta_reduction env term2))

(* Add definition to environment *)
let add_def (name, def: string * term) env =
  (name, (delta_reduction env def)) :: env

(* Evaluate a toplevel expression *)
let delta_aux env term =
  let term1 = delta_reduction env term in
  if term1 <> term then print_endline ("δ > " ^ (term_to_str term1));
  term1

let eval env expr =
  match expr with
  | Term.TopDefinition (name, value) ->
    let term = delta_aux env value in
    (add_def (name, term) env, (eval_term term))
  | TopTerm value ->
    let term = delta_aux env value in
    (env, (eval_term term))

(* Print a term and return it *)
let print term =
  print_endline ("λ > " ^ (term_to_str term)); term

(* Initial environment *)
let default_env = []
  |> add_def ("true", read_term "\\x y. x")
  |> add_def ("false", read_term "\\x y. y")
  |> add_def ("and", read_term "\\a b. ((a b) a)")
  |> add_def ("if", read_term "\\p x y -> p x y")

  |> add_def ("0", read_term "(λf x.x)")
  |> add_def ("1", read_term "(λf x.f x)")
  |> add_def ("2", read_term "(λf x.f(f x))")
  |> add_def ("3", read_term "(λf x.f(f(f x)))")
  |> add_def ("4", read_term "(λf x.f(f(f(f x))))")
  |> add_def ("5", read_term "(λf x.f(f(f(f(f x)))))")
  |> add_def ("6", read_term "(λf x.f(f(f(f(f(f x))))))")
  |> add_def ("7", read_term "(λf x.f(f(f(f(f(f(f x)))))))")
  |> add_def ("8", read_term "(λf x.f(f(f(f(f(f(f(f x))))))))")
  |> add_def ("9", read_term "(λf x.f(f(f(f(f(f(f(f(f x)))))))))")

  |> add_def ("is0", read_term "\\n -> n (\\x -> false) true")

  |> add_def ("exp", read_term "\\ m n -> n m")
  |> add_def ("sum", read_term "lambda m n f x. m f (n f x)")
  |> add_def ("mul", read_term "\\m n f -> m(n f)")

  |> add_def ("succ", read_term "\\n f x -> f (n f x)")
  |> add_def ("pred", read_term "\\n f x -> n(\\g h -> h (g f)) (\\u -> x) (\\u ->u)")

  |> add_def ("pair", read_term "lambda x y f. f x y")
  |> add_def ("first", read_term "lambda p. p true")
  |> add_def ("second", read_term "lambda p. p false")

  |> add_def ("Y", read_term "\\g -> (\\x -> g(x x))(\\x -> g(x x))")
  |> add_def ("omega", read_term "(\\x . x x) (\\ x . x x)")

  |> add_def ("G", read_term "lambda r n. if (is0 n) 1 (mul n (r (pred n)))")
  |> add_def ("fact", read_term "Y G")
