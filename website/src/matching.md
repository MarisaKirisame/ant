# Pattern Matching

Pattern matching inspects one constructor layer at a time. A case checks the outer constructor and binds its immediate fields with variables or `_`.

```ant
let head_or_zero = fun xs ->
  match xs with
  | Nil -> 0
  | Cons head _ -> head;;
```

Each branch is tried in source order. Add a catch-all branch when the input may contain constructors that earlier cases do not handle.

```ant
let is_empty = fun xs ->
  match xs with
  | Nil -> true
  | _ -> false;;
```

Use nested `match` expressions when you need to inspect a field.

```ant
let second_or_zero = fun xs ->
  match xs with
  | Nil -> 0
  | Cons _ rest ->
    match rest with
    | Nil -> 0
    | Cons second _ -> second;;
```

Function parameters and local `let` bindings are written with variable patterns in the forms shown in this guide.
