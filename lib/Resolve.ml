open Syntax

type ctx = { func : (string, unit) Hashtbl.t }

let rec resolve_expr ctx x =
  let recurse = resolve_expr ctx in
  match x with
  | Var n -> ( match Hashtbl.find ctx.func n with None -> x | Some _ -> GVar n)
  | Lam (x, y) -> Lam (x, recurse y)
  | App (f, xs) -> App (recurse f, List.map recurse xs)
  | Op (f, l, r) -> Op (f, recurse l, recurse r)
  | Match (x, cases) -> Match (recurse x, resolve_cases ctx cases)
  | If (i, t, e) -> If (recurse i, recurse t, recurse e)
  | Let (BOne (p, l), r) -> Let (BOne (p, recurse l), recurse r)
  | Ctor _ | Int _ -> x
  | _ -> failwith ("resolve_expr: " ^ show_expr x)

and resolve_cases ctx (MatchPattern cases) = MatchPattern (List.map (fun (pat, x) -> (pat, resolve_expr ctx x)) cases)

let resolve_stmt ctx stmt =
  match stmt with
  | Type _ -> stmt
  | Term (b, x) -> Term (b, resolve_expr ctx x)
  | _ -> failwith ("resolve_stmt: " ^ show_stmt stmt)

let build_ctx ctx stmt =
  match stmt with
  | Type _ -> ()
  | Term (Some (PVar f), _) -> Hashtbl.add_exn ~key:f ~data:() ctx.func
  | _ -> failwith ("build_ctx: " ^ show_stmt stmt)

let resolve (p : prog) : prog =
  let ctx = { func = Hashtbl.create (module Core.String) } in
  List.iter (build_ctx ctx) p;
  List.map (resolve_stmt ctx) p
