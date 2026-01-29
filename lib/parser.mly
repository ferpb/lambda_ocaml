%{
    open Term
%}

%token <string> ATOM
%token LPAREN RPAREN LAMBDA DOT
%token COLONEQ EOL

%start main
%type <Term.toplevel> main

%start lambda_term
%type <Term.term> lambda_term

%%

main:
| definition EOL { $1 }
| lambda_term EOL { TopTerm $1 }

definition:
| ATOM COLONEQ lambda_term { TopDefinition ($1, $3) }

// Lambda term grammar
lambda_term:
| abstraction { $1 }
| application { $1 }

simple_term:
| ATOM { Var $1}
| LPAREN lambda_term RPAREN { $2 }

abstraction:
| LAMBDA binders DOT lambda_term { List.fold_right (fun binder term -> Abs (binder, term)) (List.rev $2) $4 }

binders:
| ATOM { [$1] }
| binders ATOM { $2 :: $1 }

application:
| simple_term { $1 }
| application simple_term { App ($1, $2) }
| application abstraction { App ($1, $2) }
