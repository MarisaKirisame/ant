# Language Syntax

A `.ant` file is a sequence of top-level items. End top-level declarations with `;;`; the examples do this consistently.

## Names

Value names start with a lowercase letter or `_`. Constructors start with an uppercase letter.

```ant
let double = fun x -> x + x;;

type option_int =
  | None
  | Some of int;;
```

## Functions

Write functions as top-level `let` bindings whose body starts with `fun`. Recursive functions use `let rec`.

```ant
type nat =
  | Z
  | S of nat;;

let rec add_nat = fun lhs rhs ->
  match lhs with
  | Z -> rhs
  | S rest -> S (add_nat rest rhs);;
```

Function calls use whitespace:

```ant
let add3 = fun x -> x + 3;;
let six = fun x -> add3 (add3 x);;
```

## Expressions

\Ant expressions cover integer and boolean literals, variables, constructor values, function calls, local `let`, `if`, and infix operators.

```ant
let clamp_positive = fun x ->
  if x < 0 then 0 else x;;

let choose = fun flag lhs rhs ->
  if flag && lhs <= rhs then lhs else rhs;;
```

Supported infix operators are:

| Group | Operators |
| --- | --- |
| Boolean | `&&`, `\|\|` |
| Equality | `=` |
| Comparison | `<`, `<=`, `>`, `>=` |
| Arithmetic | `+`, `-`, `*`, `/` |

Use parentheses when precedence matters to the reader.
