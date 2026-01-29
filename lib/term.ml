(* Lambda term *)
type term =
| Var of string
| Abs of string * term
| App of term * term

(* Lambda term with De Bruijn representation *)
type debruijn =
| DBVarI of int
| DBVarS of string
| DBAbs of debruijn
| DBApp of debruijn * debruijn

(* Toplevel expressions *)
type toplevel =
| TopDefinition of string * term
| TopTerm of term


(* Transform a lambda term to De Bruijn representation *)
let term_to_debruijn (term: term): debruijn =
  let rec aux (env: (string * int) list) term =
    match term with
    | Var x -> (try DBVarI (List.assoc x env) with Not_found -> DBVarS x)
    | Abs (x, t) ->
      (* Add binding to environment *)
      let (names, counts) = List.split env in
      let new_env = (x, 1) :: (List.combine names (List.map ((+) 1) counts)) in
      DBAbs (aux new_env t)
    | App (t, s) -> DBApp ((aux env t), (aux env s))
  in aux [] term

let alpha_equivalent term1 term2 = (term_to_debruijn term1) = (term_to_debruijn term2)


(* Term printing functions *)
let rec term_to_str term =
  match term with
  | Var x -> x
  | Abs (x, t) -> "(λ" ^ x ^ "." ^ (term_to_str t) ^ ")"
  | App (Var x, Var y) -> "(" ^ x ^ " " ^ y ^ ")" (* separate both variables with whitespace *)
  | App (t, s) -> "(" ^ (term_to_str t) ^ (term_to_str s) ^ ")"

let rec debruijn_to_str term =
  match term with
  | DBVarI x -> string_of_int x
  | DBVarS x -> x
  | DBAbs t -> "(λ " ^ (debruijn_to_str t) ^ ")"
  | DBApp (t, s) -> "(" ^ (debruijn_to_str t) ^ " " ^ (debruijn_to_str s) ^ ")"


(* Beta reduction implementation *)

module SS = Set.Make(String)
let string_of_set set =
  "{" ^ (String.concat " " (SS.elements set)) ^ "}"

(* Return a list with the names of the free variables in a term *)
let free_vars term =
  let rec aux bound_vars term =
    match term with
    | Var x -> if SS.mem x bound_vars then SS.empty else SS.singleton x
    | Abs (x, t) -> aux (SS.add x bound_vars) t
    | App (t, s) -> SS.union (aux bound_vars t) (aux bound_vars s)
  in aux SS.empty term

(* Return a new name not present in 'list' by apending '_number' to 'var' *)
let new_name var list =
  let create_name name number = name ^ "_" ^ (string_of_int number) in
  let result = Seq.ints 1 |> Seq.map (create_name var) |> Seq.filter (fun n -> not (SS.mem n list)) |> Seq.uncons in
  match result with
  | None -> raise Not_found
  | Some (var1, _) -> var1

(* Rename free variables named 'var' in 'term' with 'var1' *)
let rec rename var var1 term =
  match term with
  | Var x when x = var -> Var var1
  | Var _ -> term
  | Abs (x, _) when x = var -> term (* 'var' is no longer free in 'term' *)
  | Abs (x, t) -> Abs (x, (rename var var1 t))
  | App (t, s) -> App ((rename var var1 t), (rename var var1 s))

(* Perform beta reduction term[var := term1] *)
let rec beta (var, term1) term =
  (* print_endline ("β > " ^ (term_to_str term) ^ "[" ^ var ^ " := " ^ (term_to_str term1) ^ "]"); *)
  let fvs_term1 = free_vars term1 in
  match term with
  | Var x when x = var -> term1
  | Var _ -> term

  | Abs (x, _) when x = var -> term
  | Abs (x, t) when SS.mem x fvs_term1 ->
    let x1 = new_name x (SS.union fvs_term1 (free_vars t)) in
    let t1 = (rename x x1 t) in
    (* print_endline ("α > Name conflict in " ^ (term_to_str term) ^ "[" ^ x ^ " := " ^ (term_to_str term1) ^ "], α-converted term to " ^ (term_to_str (Abs (x1, t1)))); *)
    Abs (x1, beta (var, term1) t1)
  | Abs (x, t) -> Abs (x, beta (var, term1) t)

  | App (t, s) -> App (beta (var, term1) t, beta (var, term1) s)


(* Evaluation*)

let rec is_reduced term =
  match term with
  | Var _ -> true
  | App (Abs (_, _), _) -> false
  | Abs (_, t) -> is_reduced t
  | App (t, s) -> is_reduced t && is_reduced s

let rec normal_order_step term =
  match term with
  | Var _ -> term
  | App (Abs (x, t), s) -> beta (x, s) t
  | App (t, s) -> App ((normal_order_step t), (normal_order_step s))
  | Abs (x, t) -> Abs (x, normal_order_step t)

let rec eval_term term =
  if is_reduced term then
    term
  else
    let reduced = normal_order_step term in
    print_endline ("β > " ^ (term_to_str reduced));
    eval_term reduced


(* Alternative version where the left term is fully reduced before subsituting the right term.
 * It is faster, but the sequence of reductions is more difficult to understand *)

(*
let rec normal_order_step term =
  match term with
  | App (t, s) -> (
    match normal_order_step t with
    | Abs (x, r) -> normal_order_step (beta (x, s) r)
    | _ -> term
  )
  | _ -> term

let rec eval_term term =
  match normal_order_step term with
  | Var x -> Var x
  | Abs (x, t) -> Abs (x, eval_term t)
  | App (t, s) -> App (eval_term t, eval_term s)
*)
