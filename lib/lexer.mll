{
    open Parser
}

rule token = parse
| [' ' '\t'] { token lexbuf } (* skip whitespace *)
| '\n' { EOL }
| eof { EOL }
| "λ" { LAMBDA }
| "lambda" { LAMBDA }
| '\\' { LAMBDA }
| '.' { DOT }
| "->" { DOT }
| '(' { LPAREN }
| ')' { RPAREN }
| ":=" { COLONEQ}
| ['a'-'z' 'A'-'Z' '0'-'9' '*' '+' '-' '%' '=' ',' '!' '@' '$' '\'' '_']+ as atom { ATOM atom }
