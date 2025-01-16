module Monad = struct
  module type Basic2 = sig
    type ('b, 'a) t

    val return : 'a -> ('b, 'a) t
    val bind : ('b, 'a) t -> f:('a -> ('b, 'c) t) -> ('b, 'c) t
  end

  module type Basic = sig
    type 'a t

    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end

  module Make2 (M : Basic2) = struct
    include M

    let map ma ~f = bind ma ~f:(fun a -> return (f a))
    let ( >>= ) t f = bind t ~f
    let ( >>| ) t f = map t ~f
    let join t = t >>= fun t' -> t'
    let ignore t = map t ~f:(fun _ -> ())

    module Let_syntax = struct
      module Let_syntax = struct
        let return = return
        let bind = bind
        let map = map

        let both a b =
          a >>= fun a ->
          b >>| fun b -> (a, b)

        module Open_on_rhs = struct end
      end
    end
  end

  module Make (M : Basic) = struct
    include Make2 (struct
      type ('b, 'a) t = 'a M.t

      let return = M.return
      let bind = M.bind
    end)
  end

  module Result = Make2 (struct
    type ('b, 'a) t = ('a, 'b) result

    let return a = Ok a
    let bind a ~f = match a with Ok r -> f r | Error e -> Error e
  end)

  module State_t
      (S : sig
        type t
      end)
      (M : Basic2) =
  Make2 (struct
    type ('b, 'a) t = S.t -> ('b, 'a * S.t) M.t

    module M_full = Make2 (M)
    open M_full.Let_syntax

    let return a s = M.return (a, s)

    let bind m ~f s =
      let%bind a, s' = m s in
      (f a) s'
  end)
end

module Id = struct
  type t = Var of string | TVar of int | ExVar of int

  let greek_unicode_lower = [ "α"; "β"; "γ"; "δ"; "ε"; "ζ"; "η"; "θ" ]

  let int_to_greek n =
    let rec convert n acc =
      if n < 8 then List.nth greek_unicode_lower n ^ acc
      else
        let quotient = (n / 8) - 1 in
        let remainder = n mod 8 in
        convert quotient (List.nth greek_unicode_lower remainder ^ acc)
    in
    convert n ""

  let pp = function
    | Var x -> x
    | TVar x -> int_to_greek x
    | ExVar x -> "'" ^ string_of_int x
end

module Ty = struct
  type t =
    | Unit
    | Int
    | Float
    | Bool
    | Str
    | TVar of Id.t
    | ExVar of Id.t
    | Forall of Id.t * t
    | Func of t * t

  let rec is_monotype = function
    | Forall _ -> false
    | Func (a, b) -> is_monotype a && is_monotype b
    | _ -> true

  let rec occurs_exvar ev = function
    | ExVar ev' when ev = ev' -> true
    | Forall (_, b) -> occurs_exvar ev b
    | Func (a, b) -> occurs_exvar ev a || occurs_exvar ev b
    | _ -> false

  let rec replace_when p r = function
    | t when p t -> r
    | Forall (x, b) -> Forall (x, replace_when p r b)
    | Func (a, b) -> Func (replace_when p r a, replace_when p r b)
    | t -> t

  let replace_equals p r = replace_when (( = ) p) r

  let rec generalize_exvars e = function
    | [] -> e
    | (ev, n) :: evns ->
        Forall (n, generalize_exvars (replace_equals (ExVar ev) (TVar n) e) evns)

  let rec pp = function
    | Unit -> "unit"
    | Int -> "int"
    | Float -> "float"
    | Bool -> "bool"
    | Str -> "str"
    | TVar x -> Id.pp x
    | ExVar x -> Id.pp x
    | Forall (x, b) -> "∀" ^ Id.pp x ^ "." ^ pp b
    | Func (a, b) -> "(" ^ pp a ^ ") → " ^ pp b
end

module Ctx = struct
  type e =
    | Var of Id.t * Ty.t
    | TVar of Id.t
    | ExVar of Id.t
    | Solved of Id.t * Ty.t
    | Marker of Id.t

  type t = e list

  let find_map = List.find_map
  let filter_map = List.filter_map
  let exists p = List.exists (fun ce -> Option.is_some (p ce))

  (* acc2 is returned reversely *)
  let split pred ctx =
    let rec recurse acc1 acc2 =
      match acc1 with
      | [] -> None
      | e :: es -> (
          match pred e with
          | Some _ -> Some (acc2, e, es)
          | None -> recurse es (e :: acc2))
    in
    recurse ctx []

  (* ctx3 and ctx2 is returned reversely *)
  let split2 pred2 pred1 ctx =
    match split pred2 ctx with
    | Some (ctx3, e2, ctx') -> (
        match split pred1 ctx' with
        | Some (ctx2, e1, ctx1) -> Some (ctx3, e2, ctx2, e1, ctx1)
        | None -> None)
    | None -> None

  let pp_e = function
    | Var (x, t) -> "(" ^ Id.pp x ^ " : " ^ Ty.pp t ^ ")"
    | TVar x -> Id.pp x
    | ExVar x -> Id.pp x
    | Solved (x, t) -> "(" ^ Id.pp x ^ " = " ^ Ty.pp t ^ ")"
    | Marker x -> "➤" ^ Id.pp x

  let rec pp = function [] -> "" | e :: es -> pp_e e ^ " :: " ^ pp es

  module P = struct
    let var_named x = function Var (x', b) when x = x' -> Some b | _ -> None
    let tvar_named x = function TVar x' when x = x' -> Some () | _ -> None

    let exvar_named x = function
      | ExVar x' when x = x' -> Some None
      | Solved (x', b) when x = x' -> Some (Some b)
      | _ -> None

    let exvar_unsolved = function ExVar x -> Some x | _ -> None

    let exvar_unsolved_named x = function
      | ExVar x' when x = x' -> Some ()
      | _ -> None

    let exvar_solved_named x = function
      | Solved (x', b) when x = x' -> Some b
      | _ -> None

    let marker_named x = function Marker x' when x = x' -> Some () | _ -> None
  end

  let rec is_ty_wellformed ctx = function
    | Ty.TVar x -> exists (P.tvar_named x) ctx
    | Ty.Func (a, b) -> is_ty_wellformed ctx a && is_ty_wellformed ctx b
    | Ty.Forall (x, b) -> is_ty_wellformed (TVar x :: ctx) b
    | Ty.ExVar x -> exists (P.exvar_named x) ctx
    | _ -> true

  let rec apply ctx = function
    | Ty.ExVar x -> (
        match find_map (P.exvar_solved_named x) ctx with
        | Some (Ty.ExVar x') -> apply ctx (Ty.ExVar x')
        | Some t -> apply ctx t
        | None -> Ty.ExVar x)
    | Ty.Func (a, b) -> Ty.Func (apply ctx a, apply ctx b)
    | Ty.Forall (x, a) -> Ty.Forall (x, apply ctx a)
    | t -> t
end

module Env = struct
  (* `int * int` stands for `exvar_cnt * tvar_cnt` *)
  include
    Monad.State_t
      (struct
        type t = int * int
      end)
      (Monad.Result)

  open Let_syntax

  let run s e = e s 

  let err msg (_, _) = Error msg
  let fresh_exvar (evc, tvc) = Ok (Id.ExVar evc, (evc + 1, tvc))
  let fresh_tvar (evc, tvc) = Ok (Id.TVar tvc, (evc, tvc + 1))

  let rec fresh_tvars = function
    | [] -> return []
    | ev :: evs ->
        let%bind n = fresh_tvar in
        let%bind ns' = fresh_tvars evs in
        return ((ev, n) :: ns')
end

open Env.Let_syntax

(* Under input context Γ, type A is a subtype of B, with output context Δ *)
let rec subtype ctx a b =
  (* print_endline
    [%string
      "subtype:\n  ctx=%{Ctx.show ctx}\n  a=%{show_ty a}\n  b=%{show_ty b}\n"]; *)
  match (a, b) with
  (* Unit *)
  | Ty.Unit, Ty.Unit -> Env.return ctx
  (* Var *)
  | Ty.TVar x1, Ty.TVar x2 when x1 = x2 ->
      if Ctx.exists (Ctx.P.tvar_named x1) ctx then Env.return ctx
      else Env.err [%string "unbound type variable %{Id.pp x1}"]
  (* Exvar *)
  | Ty.ExVar x1, Ty.ExVar x2 when x1 = x2 ->
      if Ctx.exists (Ctx.P.exvar_unsolved_named x1) ctx then Env.return ctx
      else Env.err [%string "unbound existential variable %{Id.pp x1}"]
  (* → *)
  | Ty.Func (a1, a2), Ty.Func (b1, b2) ->
      let%bind ctx' = subtype ctx b1 a1 in
      let%bind ctx'' = subtype ctx' (Ctx.apply ctx' a2) (Ctx.apply ctx' b2) in
      Env.return ctx''
  (* ∀L *)
  | Ty.Forall (x, a), b -> (
      let%bind ev = Env.fresh_exvar in
      let ctx' = Ctx.ExVar ev :: Ctx.Marker ev :: ctx in
      let%bind ctx'' =
        subtype ctx' (Ty.replace_equals (Ty.TVar x) (Ty.ExVar ev) a) b
      in
      match Ctx.split (Ctx.P.marker_named ev) ctx'' with
      | Some (_, _, ctx''') -> Env.return ctx'''
      | None ->
          Env.err
            [%string
              "unable to split: missing marker for existential variable \
               %{Id.pp ev}"])
  (* ∀R *)
  | a, Ty.Forall (x, b) -> (
      let ctx' = Ctx.TVar x :: ctx in
      let%bind ctx'' = subtype ctx' a b in
      match Ctx.split (Ctx.P.tvar_named x) ctx'' with
      | Some (_, _, ctx''') -> Env.return ctx'''
      | None ->
          Env.err [%string "unable to split: missing type variable %{Id.pp x}"])
  (* InstantiateL *)
  | Ty.ExVar eva, a ->
      if Ty.occurs_exvar eva a then
        Env.err [%string "found circularity of %{Id.pp eva}"]
      else if Ctx.exists (Ctx.P.exvar_unsolved_named eva) ctx then
        instl ctx eva a
      else
        Env.err
          [%string "existential variable %{Id.pp eva} is missing or solved"]
  (* InstantiateR *)
  | a, Ty.ExVar eva ->
      if Ty.occurs_exvar eva a then
        Env.err [%string "found circularity of %{Id.pp eva}"]
      else if Ctx.exists (Ctx.P.exvar_unsolved_named eva) ctx then
        instr ctx eva a
      else
        Env.err
          [%string "existential variable %{Id.pp eva} is missing or solved"]
  | _ ->
      Env.err
        [%string "subtype: no rule applicable for %{Ty.pp a} <: %{Ty.pp b}"]

(* Under input context Γ, instantiate α-hat such that α-hat <: A, with output context Δ *)
and instl ctx eva t =
  let instl_solve t =
    match Ctx.split (Ctx.P.exvar_unsolved_named eva) ctx with
    | Some (ctx2, _, ctx1) ->
        if Ctx.is_ty_wellformed ctx1 t then
          Env.return (List.rev_append ctx2 (Ctx.Solved (eva, t) :: ctx1))
        else Env.err [%string "non wellformed type %{Ty.pp t}"]
    | None ->
        Env.err
          [%string
            "unable to split: %{Id.pp eva} is unbound or solved\n\
             ctx=%{Ctx.pp ctx}"]
  in
  (* print_endline
    [%string "instl:\n  ctx=%{Ctx.show ctx}\n  eva=%{eva}\n  t=%{show_ty t}\n"]; *)
  match t with
  (* InstLReach *)
  | Ty.ExVar evb -> (
      match
        Ctx.split2
          (Ctx.P.exvar_unsolved_named evb)
          (Ctx.P.exvar_unsolved_named eva)
          ctx
      with
      | Some (ctx3, _, ctx2, e1, ctx1) ->
          Env.return
            (List.rev_append ctx3
               (Ctx.Solved (evb, Ty.ExVar eva)
               :: List.rev_append ctx2 (e1 :: ctx1)))
      (* evb cannot be generalized further *)
      | None -> instl_solve t)
  (* InstLArr *)
  | Ty.Func (a1, a2) -> (
      match Ctx.split (Ctx.P.exvar_unsolved_named eva) ctx with
      | Some (ctx2, _, ctx1) ->
          let%bind ev1 = Env.fresh_exvar in
          let%bind ev2 = Env.fresh_exvar in
          let ctx' =
            List.rev_append ctx2
              (Ctx.Solved (eva, Ty.Func (Ty.ExVar ev1, Ty.ExVar ev2))
              :: Ctx.ExVar ev1 :: Ctx.ExVar ev2 :: ctx1)
          in
          let%bind ctx'' = instr ctx' ev1 a1 in
          let%bind ctx''' = instl ctx'' ev2 (Ctx.apply ctx'' a2) in
          Env.return ctx'''
      | None ->
          Env.err
            [%string
              "unable to split: unbound existential variable %{Id.pp eva}"])
  (* InstLAllR *)
  | Ty.Forall (tvb, b) ->
      if Ctx.exists (Ctx.P.exvar_unsolved_named eva) ctx then
        let ctx' = Ctx.TVar tvb :: ctx in
        let%bind ctx'' = instl ctx' eva b in
        match Ctx.split (Ctx.P.tvar_named tvb) ctx'' with
        | Some (_, _, ctx1) -> Env.return ctx1
        | None ->
            Env.err
              [%string "unable to split: missing type variable %{Id.pp tvb}"]
      else Env.err [%string "unbound existential variable %{Id.pp eva}"]
  (* InstLSolve *)
  | t when Ty.is_monotype t -> instl_solve t
  | t -> Env.err [%string "instl: no rule applicable for %{Ty.pp t}"]

(* Under input context Γ, instantiate α-hat such that A <: α-hat, with output context Δ *)
and instr ctx eva t =
  let instr_solve t =
    match Ctx.split (Ctx.P.exvar_unsolved_named eva) ctx with
    | Some (ctx2, _, ctx1) ->
        if Ctx.is_ty_wellformed ctx1 t then
          Env.return (List.rev_append ctx2 (Ctx.Solved (eva, t) :: ctx1))
        else Env.err [%string "non wellformed type %{Ty.pp t}"]
    | None ->
        Env.err
          [%string
            "unable to split: %{Id.pp eva} is unbound or solved\n\
             ctx=%{Ctx.pp ctx}"]
  in
  (* print_endline
    [%string "instr:\n  ctx=%{Ctx.show ctx}\n  eva=%{eva}\n  t=%{show_ty t}\n"]; *)
  match t with
  (* InstRReach *)
  | Ty.ExVar evb -> (
      match
        Ctx.split2
          (Ctx.P.exvar_unsolved_named evb)
          (Ctx.P.exvar_unsolved_named eva)
          ctx
      with
      | Some (ctx3, _, ctx2, e1, ctx1) ->
          Env.return
            (List.rev_append ctx3
               (Ctx.Solved (evb, Ty.ExVar eva)
               :: List.rev_append ctx2 (e1 :: ctx1)))
      (* evb cannot be generalized further *)
      | None -> instr_solve t)
  (* InstRArr *)
  | Ty.Func (a1, a2) -> (
      match Ctx.split (Ctx.P.exvar_unsolved_named eva) ctx with
      | Some (ctx2, _, ctx1) ->
          let%bind ev1 = Env.fresh_exvar in
          let%bind ev2 = Env.fresh_exvar in
          let ctx' =
            List.rev_append ctx2
              (Ctx.Solved (eva, Ty.Func (Ty.ExVar ev1, Ty.ExVar ev2))
              :: Ctx.ExVar ev1 :: Ctx.ExVar ev2 :: ctx1)
          in
          let%bind ctx'' = instl ctx' ev1 a1 in
          let%bind ctx''' = instr ctx'' ev2 (Ctx.apply ctx'' a2) in
          Env.return ctx'''
      | None ->
          Env.err
            [%string
              "unable to split: unbound existential variable %{Id.pp eva}"])
  (* InstRAllL *)
  | Ty.Forall (tvb, b) ->
      if Ctx.exists (Ctx.P.exvar_unsolved_named eva) ctx then
        let ctx' = Ctx.ExVar tvb :: Ctx.Marker tvb :: ctx in
        (* TODO: investigate the same name of tvar and exvar *)
        let%bind ctx'' =
          instr ctx' eva (Ty.replace_equals (Ty.TVar tvb) (Ty.ExVar tvb) b)
        in
        match Ctx.split (Ctx.P.marker_named tvb) ctx'' with
        | Some (_, _, ctx1) -> Env.return ctx1
        | None ->
            Env.err
              [%string
                "unable to split: missing marker for existential variable \
                 %{Id.pp tvb}"]
      else Env.err [%string "unbound existential variable %{Id.pp eva}"]
  (* InstRSolve *)
  | t when Ty.is_monotype t -> instr_solve t
  | t -> Env.err [%string "instr: no rule applicable for %{Ty.pp t}"]

(* Under input context Γ, e checks against input type A, with output context Δ *)
let rec check ctx e ta =
  (* print_endline
    [%string
      "check:\n\
      \  ctx=%{Ctx.show ctx}\n\
      \  e=%{Syntax.show_expr e}\n\
      \  ta=%{show_ty ta}\n"]; *)
  match (e, ta) with
  (* 1I *)
  | Syntax.Unit, Ty.Unit -> Env.return ctx
  (* ∀I *)
  | e, Ty.Forall (tva, a) -> (
      let%bind ctx' = check (Ctx.TVar tva :: ctx) e a in
      match Ctx.split (Ctx.P.tvar_named tva) ctx' with
      | Some (_, _, ctx1) -> Env.return ctx1
      | None ->
          Env.err
            [%string "unable to split: missing type variable %{Id.pp tva}"])
  (* →I *)
  | Syntax.Lam ([ Syntax.PVar x ], e), Ty.Func (ta, tb) -> (
      let%bind ctx' = check (Ctx.Var (Id.Var x, ta) :: ctx) e tb in
      match Ctx.split (Ctx.P.var_named (Id.Var x)) ctx' with
      | Some (_, _, ctx1) -> Env.return ctx1
      | None -> Env.err [%string "unable to split: missing variable %{x}"])
  (* Sub *)
  | e, b ->
      let%bind a, ctx' = infer ctx e in
      subtype ctx' (Ctx.apply ctx' a) (Ctx.apply ctx' b)

(* Under input context Γ, e synthesizes output type A, with output context Δ *)
and infer ctx e =
  (* print_endline
    [%string "infer:\n  ctx=%{Ctx.show ctx}\n  e=%{Syntax.show_expr e}\n"]; *)
  match e with
  (* 1I⇒ *)
  | Syntax.Unit -> Env.return (Ty.Unit, ctx)
  (* Var *)
  | Syntax.Var x -> (
      match Ctx.find_map (Ctx.P.var_named (Id.Var x)) ctx with
      | Some t -> Env.return (t, ctx)
      | None -> Env.err [%string "unbound variable %{x}"])
  (* →I⇒ *)
  (* | Syntax.Lam ([ Syntax.PVar x ], e) -> (
      let%bind eva = Env.fresh_exvar in
      let%bind evb = Env.fresh_exvar in
      let ctx' =
        Ctx.CEVar (x, TExVar eva) :: Ctx.CEExVar evb :: Ctx.CEExVar eva :: ctx
      in
      let%bind ctx'' = check ctx' e (TExVar evb) in
      match Ctx.split (Ctx.var_named x) ctx'' with
      | Some (_, _, ctx1) -> Env.return (TLam (TExVar eva, TExVar evb), ctx1)
      | None ->
          Env.non_wellformed_context
            [%string "unable to split: missing variable %{x}"]) *)
  (* →I⇒' *)
  | Syntax.Lam ([ Syntax.PVar x ], e) -> (
      let%bind eva = Env.fresh_exvar in
      let%bind evb = Env.fresh_exvar in
      let ctx' =
        Ctx.Var (Id.Var x, Ty.ExVar eva)
        :: Ctx.ExVar evb :: Ctx.ExVar eva :: Ctx.Marker eva :: ctx
      in
      let%bind ctx'' = check ctx' e (Ty.ExVar evb) in
      match Ctx.split (Ctx.P.marker_named eva) ctx'' with
      | Some (ctx2, _, ctx1) ->
          let mt = Ctx.apply ctx2 (Ty.Func (Ty.ExVar eva, Ty.ExVar evb)) in
          let evs = Ctx.filter_map Ctx.P.exvar_unsolved ctx2 in
          let%bind evns = Env.fresh_tvars evs in
          if Ty.is_monotype mt then
            Env.return (Ty.generalize_exvars mt evns, ctx1)
          else
            Env.err [%string "infer: →I⇒' failed on non-monotype %{Ty.pp mt}"]
      | None ->
          Env.err
            [%string
              "unable to split: missing marker for existential variable \
               %{Id.pp eva}"])
  (* →E *)
  | Syntax.App (e1, [ e2 ]) ->
      let%bind a, ctx' = infer ctx e1 in
      infer_app ctx' (Ctx.apply ctx' a) e2
  | t -> Env.err [%string "infer: no rule applicable for %{Syntax.show_expr t}"]

(* Under input context Γ, applying a function of type Ato e synthesizes type C, with output context Δ *)
and infer_app ctx ta e =
  (* print_endline
    [%string
      "infer_app:\n\
      \  ctx=%{Ctx.show ctx}\n\
      \  e=%{Syntax.show_expr e}\n\
      \  ta=%{show_ty ta}\n"]; *)
  match ta with
  (* ∀App *)
  | Ty.Forall (tva, a) ->
      infer_app (Ctx.ExVar tva :: ctx)
        (Ty.replace_equals (Ty.TVar tva) (Ty.ExVar tva) a)
        e
  (* α-hat App *)
  | Ty.ExVar eva -> (
      match Ctx.split (Ctx.P.exvar_unsolved_named eva) ctx with
      | Some (ctx2, _, ctx1) ->
          let%bind eva1 = Env.fresh_exvar in
          let%bind eva2 = Env.fresh_exvar in
          let ctx' =
            List.rev_append ctx2
              (Ctx.Solved (eva, Ty.Func (Ty.ExVar eva1, Ty.ExVar eva2))
              :: Ctx.ExVar eva1 :: Ctx.ExVar eva2 :: ctx1)
          in
          let%bind ctx'' = check ctx' e (Ty.ExVar eva1) in
          Env.return (Ty.ExVar eva2, ctx'')
      | None ->
          Env.err
            [%string
              "unable to split: unbound existential variable %{Id.pp eva}"])
  (* →App *)
  | Ty.Func (a, c) ->
      let%bind ctx' = check ctx e a in
      Env.return (c, ctx')
  | t -> Env.err [%string "infer_app: no rule applicable for %{Ty.pp t}"]
