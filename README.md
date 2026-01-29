# Lambda calculus interpreter in OCaml

Interpreter for untyped lambda calculus. Written for a summer course at Universidad de Zaragoza, 2023.

Documentation in Spanish: `memoria.pdf`.

## Build

```sh
dune build
```

## Run (REPL)

```sh
rlwrap _build/default/bin/main.exe
```

The interpreter reads terms from stdin and shows the reduction steps.

Example session:

```
# sum := lambda m n f x. m f (n f x)
λ > (λm.(λn.(λf.(λx.((m f)((n f)x))))))
α > sum
# 1 := (lambda f x. f x)
λ > (λf.(λx.(f x)))
α > 1
# 2 := (lambda f x. f(f x))
λ > (λf.(λx.(f(f x))))
α > 2
# sum 1 1
δ > (((λm.(λn.(λf.(λx.((m f)((n f)x))))))(λf.(λx.(f x))))(λf.(λx.(f x))))
β > ((λn.(λf.(λx.(((λf.(λx.(f x)))f)((n f)x)))))(λf.(λx.(f x))))
β > (λf.(λx.(((λf.(λx.(f x)))f)(((λf.(λx.(f x)))f)x))))
β > (λf.(λx.((λx.(f x))((λx.(f x))x))))
β > (λf.(λx.(f((λx.(f x))x))))
β > (λf.(λx.(f(f x))))
λ > (λf.(λx.(f(f x))))
α > 2
```

## Tests

```sh
dune test
```
