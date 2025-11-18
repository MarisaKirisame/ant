open Syntax
module Hashtbl = Core.Hashtbl

type ctx = { func : (string, unit) Hashtbl.t }

let rec resolve_expr ctx x =
  let recurse = resolve_expr ctx in
  match x with
  | Var (n, info) -> ( match Hashtbl.find ctx.func n with None -> x | Some _ -> GVar (n, info))
  | Lam (x, y, info) -> Lam (x, recurse y, info)
  | App (f, xs, info) -> App (recurse f, List.map recurse xs, info)
  | Op (f, l, r, info) -> Op (f, recurse l, recurse r, info)
  | Match (x, cases, info) -> Match (recurse x, resolve_cases ctx cases, info)
  | If (i, t, e, info) -> If (recurse i, recurse t, recurse e, info)
  | Let (BOne (p, l), r, info) -> Let (BOne (p, recurse l), recurse r, info)
  | Ctor _ | Int _ -> x
  | _ -> failwith ("resolve_expr: " ^ Syntax.string_of_document @@ Syntax.pp_expr x)

and resolve_cases ctx (MatchPattern cases) = MatchPattern (List.map (fun (pat, x) -> (pat, resolve_expr ctx x)) cases)

let resolve_stmt ctx stmt =
  match stmt with
  | Type _ -> stmt
  | Term (b, x, info) -> Term (b, resolve_expr ctx x, info)
  | _ -> failwith ("resolve_stmt: " ^ Syntax.string_of_document @@ Syntax.pp_stmt stmt)

let build_ctx ctx stmt =
  match stmt with
  | Type _ -> ()
  | Term (Some (PVar (f, _)), _, _) -> Hashtbl.add_exn ~key:f ~data:() ctx.func
  | _ -> failwith ("build_ctx: " ^ Syntax.string_of_document @@ Syntax.pp_stmt stmt)

let resolve (p : 'a prog) : 'a prog =
  let ctx = { func = Hashtbl.create (module Core.String) } in
  List.iter (build_ctx ctx) (fst p);
  (List.map (resolve_stmt ctx) (fst p), snd p)
