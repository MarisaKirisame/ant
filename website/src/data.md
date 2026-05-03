# Data and Types

\Ant programs define algebraic data types with `type`. Constructors either have no payload or a fixed payload written after `of`.

```ant
type nat =
  | Z
  | S of nat;;

type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr;;
```

Constructor applications use the constructor followed by its fields.

```ant
let one = fun unused -> S Z;;
let simple_expr = fun unused -> Add (Const 1) (Const 2);;
```

## Type Parameters

Parameterized types put type variables after the type name.

```ant
type list 'a =
  | Nil
  | Cons of 'a * list 'a;;

type option 'a =
  | None
  | Some of 'a;;
```

The type grammar recognizes `int`, `bool`, `float`, `unit`, named types, type variables such as `'a`, type application, and function arrows. Current examples mainly use data declarations plus inferred function types, rather than source annotations on values.
