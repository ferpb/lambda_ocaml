open Liblambda.Term
open Liblambda.Toplevel

let () =
  (* read term *)
  assert (read_term "\\x y. y x" = Abs ("x", Abs ("y", App (Var "y", Var "x"))));

  (* Wikipedia examples *)
  assert (term_to_debruijn (read_term "\\x y z. x z(y z)") =
          DBAbs (DBAbs (DBAbs (DBApp (DBApp (DBVarI 3, DBVarI 1), DBApp (DBVarI 2, DBVarI 1))))));
  assert (term_to_debruijn (read_term "\\z. (\\y. y(\\x. x)) (\\x. z x)") =
          DBAbs (DBApp (DBAbs (DBApp (DBVarI 1, DBAbs (DBVarI 1))), DBAbs (DBApp (DBVarI 2, DBVarI 1)))));

  (* De Bruijn with free variables *)
  assert (term_to_debruijn (read_term "\\x. x y") = DBAbs (DBApp (DBVarI 1, DBVarS "y")));
  assert (term_to_debruijn (read_term "lambda x . x lambda y. x y z)") =
          DBAbs (DBApp (DBVarI 1, DBAbs (DBApp (DBApp (DBVarI 2, DBVarI 1), DBVarS "z")))));

  (* free vars *)
  assert (free_vars (read_term "(\\ x y. b \\ x . a) c") = SS.of_list ["b"; "a"; "c"]);
  assert (free_vars (read_term "(\\ z y. z \\ x . x) c") = SS.of_list ["c"]);

  (* new name *)
  assert (new_name "x" SS.empty = "x_1");
  assert (new_name "x" (SS.of_list ["a"; "b"]) = "x_1");
  assert (new_name "x" (SS.of_list ["x_1"; "a"; "b"]) = "x_2");
  assert (new_name "x" (free_vars (read_term "x_1\\x.x y x_2")) = "x_3");

  (* rename *)
  assert (rename "a" "x" (read_term "x(x\\y. x\\x.y x)x") = read_term "x(x\\y.x\\x.y x)x");
  assert (rename "x" "x_1" (read_term "x(x\\y. x\\x.y x)x") = read_term "x_1(x_1\\y.x_1\\x.y x)x_1");

  (* beta *)
  assert (beta ("x", read_term "x") (read_term "\\y.\\x.y") = read_term "\\y x.y");
  assert (beta ("y", read_term "\\x.x") (read_term "(\\ y. x \\ y . x y)") = read_term "(\\y. x\\y. x y)");
  assert (beta ("x", read_term "\\x.x") (read_term "(\\ y. x \\ x . x y)") = read_term "(\\y. (\\x.x)\\x. x y)");

  (* misc errors *)
  assert (beta ("x", read_term "\\x.x") (read_term "(\\ y. x \\ x . x y)") = read_term "(\\y. (\\x.x)\\x. x y)");
  assert (eval_term (read_term "\\x. (\\y.\\x.y) x") = read_term "\\x.\\x_1.x");
  assert (eval_term (read_term "(\\x.(\\y.(x(\\x.x y))))y") = read_term "\\y_1.(y(\\x.(x y_1)))");

  (* eval term *)
  assert (eval_term (read_term "(\\p x y. p x y) (\\x y. x) a ((\\x.x x) (\\x.x x))") = read_term "a")
