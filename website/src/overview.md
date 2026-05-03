# \Ant

\Ant is a small, typed functional language used by this repository to generate OCaml for the memoised CEK evaluator. Programs are written with algebraic data types, top-level functions, local bindings, integer and boolean expressions, conditionals, constructor calls, and pattern matching over one constructor layer at a time.

Start with these forms:

| Form | Example |
| --- | --- |
| Algebraic data | `type list = Nil \| Cons of int * list` |
| Functions | `let rec length = fun xs -> ...` |
| Local bindings | `let x = 1 + 2 in x` |
| Conditionals | `if x < 0 then 0 else x` |
| Constructor calls | `Cons head tail` |
| Single-level matching | `match xs with \| Nil -> ... \| Cons h t -> ...` |

```ant
type int_list =
  | Nil
  | Cons of int * int_list;;

let rec length = fun xs ->
  match xs with
  | Nil -> 0
  | Cons _ rest -> 1 + length rest;;
```

Build the generated OCaml with the default memo backend:

```bash
dune exec \Ant -- examples/List.ant generated/ListCEK.ml --compile --backend memo
```

## How Memoisation Works

The memo backend runs the program on a CEK machine, which keeps the current expression, environment, and continuation as the execution state. As the program runs, \Ant records small reusable steps from one state to the next.

On a later run, the evaluator compares the live state with the recorded states. When the current state has the same shape, \Ant reuses the recorded result and skips directly to the later state instead of evaluating each step again.

This is useful when an input changes only partly. The unchanged prefix of the computation can be reused, while the changed part still runs normally.
