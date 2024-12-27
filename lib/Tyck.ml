type ty =
  | TUnit
  | TVar of string
  | TExVar of string
  | TForall of string * ty
  | TLam of ty * ty
[@@deriving show]

let rec is_monotype = function
  | TForall (_, _) -> false
  | TLam (a, b) -> is_monotype a && is_monotype b
  | _ -> true

let rec occur_check ev = function
  | TExVar ev' when String.equal ev ev' -> true
  | TForall (_, b) -> occur_check ev b
  | TLam (a, b) -> occur_check ev a || occur_check ev b
  | _ -> false

let rec replace_tvar n ev = function
  | TVar n' when String.equal n n' -> TExVar ev
  | TLam (a, b) -> TLam (replace_tvar n ev a, replace_tvar n ev b)
  | TForall (x, b) when not (String.equal x n) ->
      TForall (x, replace_tvar n ev b)
  | t -> t

let rec replace_exvar ev n = function
  | TExVar ev' when String.equal ev ev' -> TVar n
  | TLam (a, b) -> TLam (replace_exvar ev n a, replace_exvar ev n b)
  | TForall (x, b) when not (String.equal x n) ->
      TForall (x, replace_exvar ev n b)
  | t -> t

let rec gen_exvars e = function
  | [] -> e
  | (ev, n) :: evns -> TForall (n, gen_exvars (replace_exvar ev n e) evns)

module Ctx = struct
  type entry =
    | CETVar of string
    | CEVar of string * ty
    | CEExVar of string
    | CESolved of string * ty
    | CEMarker of string
  [@@deriving show]

  type t = entry list [@@deriving show]

  let find_map = List.find_map
  let filter_map = List.filter_map
  let exists pred = List.exists (fun e -> Option.is_some (pred e))

  (*
  first returned list is in reverse order 
  make sure to use List.rev_append    
  *)
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

  (*
  the first and second returned lists are in reverse order 
  make sure to use List.rev_append    
  *)
  let split2 pred2 pred1 ctx =
    match split pred2 ctx with
    | Some (ctx3, e2, ctx') -> (
        match split pred1 ctx' with
        | Some (ctx2, e1, ctx1) -> Some (ctx3, e2, ctx2, e1, ctx1)
        | None -> None)
    | None -> None

  let var_named x = function
    | CEVar (x', b) when String.equal x x' -> Some b
    | _ -> None

  let tvar_named x = function
    | CETVar x' when String.equal x x' -> Some ()
    | _ -> None

  let exvar_named x = function
    | CEExVar x' when String.equal x x' -> Some None
    | CESolved (x', b) when String.equal x x' -> Some (Some b)
    | _ -> None

  let exvar_unsolved = function CEExVar x -> Some x | _ -> None

  let exvar_unsolved_named x = function
    | CEExVar x' when String.equal x x' -> Some ()
    | _ -> None

  let exvar_solved_named x = function
    | CESolved (x', b) when String.equal x x' -> Some b
    | _ -> None

  let marker_named x = function
    | CEMarker x' when String.equal x x' -> Some ()
    | _ -> None

  let rec is_wellformed_ty ctx = function
    | TVar x -> exists (tvar_named x) ctx
    | TLam (a, b) -> is_wellformed_ty ctx a && is_wellformed_ty ctx b
    | TForall (x, b) -> is_wellformed_ty (CETVar x :: ctx) b
    | TExVar x -> exists (exvar_named x) ctx
    | _ -> true

  let rec apply ctx = function
    | TExVar x -> (
        match find_map (exvar_solved_named x) ctx with
        | Some (TExVar x') -> apply ctx (TExVar x')
        | Some t -> t
        | None -> TExVar x)
    | TLam (a, b) -> TLam (apply ctx a, apply ctx b)
    | TForall (x, a) -> TForall (x, apply ctx a)
    | t -> t
end

module Env = struct
  type 'a r =
    | Success of 'a
    | NonWellformedContext of string
    | NoRuleApplicable of string
    | FoundCircularity of string

  (* exvar_cnt, tvar_cnt *)
  type s = int * int
  type 'a t = s -> 'a r * s

  let return x s = (Success x, s)

  let bind x ~f s =
    let a, s' = x s in
    match a with
    | Success r -> f r s'
    | NonWellformedContext m -> (NonWellformedContext m, s')
    | NoRuleApplicable m -> (NoRuleApplicable m, s')
    | FoundCircularity m -> (FoundCircularity m, s')

  let map x ~f s =
    let a, s' = x s in
    match a with
    | Success r -> (f r, s')
    | NonWellformedContext m -> (NonWellformedContext m, s')
    | NoRuleApplicable m -> (NoRuleApplicable m, s')
    | FoundCircularity m -> (FoundCircularity m, s')

  let or_else x y s =
    let a, s' = x s in
    match a with Success r -> (Success r, s') | _ -> y s

  module Let_syntax = struct
    module Let_syntax = struct
      let return = return
      let bind = bind
      let map = map

      module Open_on_rhs = struct
        let return = return
      end
    end
  end

  open Let_syntax

  let fresh_exvar (evc, tvc) =
    (Success ("ev" ^ string_of_int evc), (evc + 1, tvc))

  let fresh_tvar (evc, tvc) = (Success ("t" ^ string_of_int tvc), (evc, tvc + 1))

  let rec fresh_tvars = function
    | [] -> return []
    | ev :: evs ->
        let%bind n = fresh_tvar in
        let%bind ns' = fresh_tvars evs in
        return ((ev, n) :: ns')

  let non_wellformed_context m s = (NonWellformedContext m, s)
  let no_rule_applicable m s = (NoRuleApplicable m, s)
  let found_circularity m s = (FoundCircularity m, s)
  let run s e = fst (e s)
end

open Env.Let_syntax

(* Under input context Γ, type A is a subtype of B, with output context Δ *)
let rec subtype ctx a b =
  print_endline
    [%string
      "subtype:\n  ctx=%{Ctx.show ctx}\n  a=%{show_ty a}\n  b=%{show_ty b}\n"];
  match (a, b) with
  (* Unit *)
  | TUnit, TUnit -> Env.return ctx
  (* Var *)
  | TVar x1, TVar x2 when String.equal x1 x2 ->
      if Ctx.exists (Ctx.tvar_named x1) ctx then Env.return ctx
      else Env.non_wellformed_context [%string "unbound type variable %{x1}"]
  (* Exvar *)
  | TExVar x1, TExVar x2 when String.equal x1 x2 ->
      if Ctx.exists (Ctx.exvar_unsolved_named x1) ctx then Env.return ctx
      else
        Env.non_wellformed_context
          [%string "unbound existential variable %{x1}"]
  (* → *)
  | TLam (a1, a2), TLam (b1, b2) ->
      let%bind ctx' = subtype ctx b1 a1 in
      let%bind ctx'' = subtype ctx' (Ctx.apply ctx' a2) (Ctx.apply ctx' b2) in
      Env.return ctx''
  (* ∀L *)
  | TForall (x, a), b -> (
      let%bind ev = Env.fresh_exvar in
      let ctx' = Ctx.CEExVar ev :: Ctx.CEMarker ev :: ctx in
      let%bind ctx'' = subtype ctx' (replace_tvar x ev a) b in
      match Ctx.split (Ctx.marker_named ev) ctx'' with
      | Some (_, _, ctx''') -> Env.return ctx'''
      | None ->
          Env.non_wellformed_context
            [%string
              "unable to split: missing marker for existential variable %{ev}"])
  (* ∀R *)
  | a, TForall (x, b) -> (
      let ctx' = Ctx.CETVar x :: ctx in
      let%bind ctx'' = subtype ctx' a b in
      match Ctx.split (Ctx.tvar_named x) ctx'' with
      | Some (_, _, ctx''') -> Env.return ctx'''
      | None ->
          Env.non_wellformed_context
            [%string "unable to split: missing type variable %{x}"])
  (* InstantiateL *)
  | TExVar eva, a ->
      if occur_check eva a then
        Env.found_circularity [%string "found circularity of %{eva}"]
      else if Ctx.exists (Ctx.exvar_unsolved_named eva) ctx then instl ctx eva a
      else
        Env.non_wellformed_context
          [%string "existential variable %{eva} is missing or solved"]
  (* InstantiateR *)
  | a, TExVar eva ->
      if occur_check eva a then
        Env.found_circularity [%string "found circularity of %{eva}"]
      else if Ctx.exists (Ctx.exvar_unsolved_named eva) ctx then instr ctx eva a
      else
        Env.non_wellformed_context
          [%string "existential variable %{eva} is missing or solved"]
  | _ ->
      Env.no_rule_applicable
        [%string "subtype: no rule applicable for %{show_ty a} <: %{show_ty b}"]

(* Under input context Γ, instantiate α-hat such that α-hat <: A, with output context Δ *)
and instl ctx eva t =
  print_endline
    [%string "instl:\n  ctx=%{Ctx.show ctx}\n  eva=%{eva}\n  t=%{show_ty t}\n"];
  match t with
  (* InstLReach *)
  | TExVar evb -> (
      match
        Ctx.split2
          (Ctx.exvar_unsolved_named evb)
          (Ctx.exvar_unsolved_named eva)
          ctx
      with
      | Some (ctx3, _, ctx2, e1, ctx1) ->
          Env.return
            (List.rev_append ctx3
               (Ctx.CESolved (evb, TExVar eva)
               :: List.rev_append ctx2 (e1 :: ctx1)))
      | None ->
          Env.non_wellformed_context
            [%string
              "unable to split2: existential variable %{eva}, %{evb} is \
               unbound or solved\n\
               ctx=%{Ctx.show ctx}"])
  (* InstLArr *)
  | TLam (a1, a2) -> (
      match Ctx.split (Ctx.exvar_unsolved_named eva) ctx with
      | Some (ctx2, _, ctx1) ->
          let%bind ev1 = Env.fresh_exvar in
          let%bind ev2 = Env.fresh_exvar in
          let ctx' =
            List.rev_append ctx2
              (Ctx.CESolved (eva, TLam (TExVar ev1, TExVar ev2))
              :: Ctx.CEExVar ev1 :: Ctx.CEExVar ev2 :: ctx1)
          in
          let%bind ctx'' = instr ctx' ev1 a1 in
          let%bind ctx''' = instl ctx'' ev2 (Ctx.apply ctx'' a2) in
          Env.return ctx'''
      | None ->
          Env.non_wellformed_context
            [%string "unable to split: unbound existential variable %{eva}"])
  (* InstLAllR *)
  | TForall (tvb, b) ->
      if Ctx.exists (Ctx.exvar_unsolved_named eva) ctx then
        let ctx' = Ctx.CETVar tvb :: ctx in
        let%bind ctx'' = instl ctx' eva b in
        match Ctx.split (Ctx.tvar_named tvb) ctx'' with
        | Some (_, _, ctx1) -> Env.return ctx1
        | None ->
            Env.non_wellformed_context
              [%string "unable to split: missing type variable %{tvb}"]
      else
        Env.non_wellformed_context
          [%string "unbound existential variable %{eva}"]
  (* InstLSolve *)
  | t when is_monotype t -> (
      match Ctx.split (Ctx.exvar_unsolved_named eva) ctx with
      | Some (ctx2, _, ctx1) ->
          if Ctx.is_wellformed_ty ctx1 t then
            Env.return (List.rev_append ctx2 (Ctx.CESolved (eva, t) :: ctx1))
          else
            Env.non_wellformed_context
              [%string "non wellformed type %{show_ty t}"]
      | None ->
          Env.non_wellformed_context
            [%string
              "unable to split: %{eva} is unbound or solved\n\
               ctx=%{Ctx.show ctx}"])
  | t ->
      Env.no_rule_applicable
        [%string "instl: no rule applicable for %{show_ty t}"]

(* Under input context Γ, instantiate α-hat such that A <: α-hat, with output context Δ *)
and instr ctx eva t =
  print_endline
    [%string "instr:\n  ctx=%{Ctx.show ctx}\n  eva=%{eva}\n  t=%{show_ty t}\n"];
  match t with
  (* InstRReach *)
  | TExVar evb -> (
      match
        Ctx.split2
          (Ctx.exvar_unsolved_named evb)
          (Ctx.exvar_unsolved_named eva)
          ctx
      with
      | Some (ctx3, _, ctx2, e1, ctx1) ->
          Env.return
            (List.rev_append ctx3
               (Ctx.CESolved (evb, TExVar eva)
               :: List.rev_append ctx2 (e1 :: ctx1)))
      | None ->
          Env.non_wellformed_context
            [%string
              "unable to split2: existential variable %{eva}, %{evb} is \
               unbound or solved\n\
               ctx=%{Ctx.show ctx}"])
  (* InstRArr *)
  | TLam (a1, a2) -> (
      match Ctx.split (Ctx.exvar_unsolved_named eva) ctx with
      | Some (ctx2, _, ctx1) ->
          let%bind ev1 = Env.fresh_exvar in
          let%bind ev2 = Env.fresh_exvar in
          let ctx' =
            List.rev_append ctx2
              (Ctx.CESolved (eva, TLam (TExVar ev1, TExVar ev2))
              :: Ctx.CEExVar ev1 :: Ctx.CEExVar ev2 :: ctx1)
          in
          let%bind ctx'' = instl ctx' ev1 a1 in
          let%bind ctx''' = instr ctx'' ev2 (Ctx.apply ctx'' a2) in
          Env.return ctx'''
      | None ->
          Env.non_wellformed_context
            [%string "unable to split: unbound existential variable %{eva}"])
  (* InstRAllL *)
  | TForall (tvb, b) ->
      if Ctx.exists (Ctx.exvar_unsolved_named eva) ctx then
        let ctx' = Ctx.CEExVar tvb :: Ctx.CEMarker tvb :: ctx in
        let%bind ctx'' = instr ctx' eva (replace_tvar tvb tvb b) in
        match Ctx.split (Ctx.marker_named tvb) ctx'' with
        | Some (_, _, ctx1) -> Env.return ctx1
        | None ->
            Env.non_wellformed_context
              [%string
                "unable to split: missing marker for existential variable \
                 %{tvb}"]
      else
        Env.non_wellformed_context
          [%string "unbound existential variable %{eva}"]
  (* InstRSolve *)
  | t when is_monotype t -> (
      match Ctx.split (Ctx.exvar_unsolved_named eva) ctx with
      | Some (ctx2, _, ctx1) ->
          if Ctx.is_wellformed_ty ctx1 t then
            Env.return (List.rev_append ctx2 (Ctx.CESolved (eva, t) :: ctx1))
          else
            Env.non_wellformed_context
              [%string "non wellformed type %{show_ty t}"]
      | None ->
          Env.non_wellformed_context
            [%string
              "unable to split: %{eva} is unbound or solved\n\
               ctx=%{Ctx.show ctx}"])
  | t ->
      Env.no_rule_applicable
        [%string "instr: no rule applicable for %{show_ty t}"]

(* Under input context Γ, e checks against input type A, with output context Δ *)
let rec check ctx e ta =
  print_endline
    [%string
      "check:\n\
      \  ctx=%{Ctx.show ctx}\n\
      \  e=%{Syntax.show_expr e}\n\
      \  ta=%{show_ty ta}\n"];
  match (e, ta) with
  (* 1I *)
  | Syntax.Unit, TUnit -> Env.return ctx
  (* ∀I *)
  | e, TForall (tva, a) -> (
      let%bind ctx' = check (Ctx.CETVar tva :: ctx) e a in
      match Ctx.split (Ctx.tvar_named tva) ctx' with
      | Some (_, _, ctx1) -> Env.return ctx1
      | None ->
          Env.non_wellformed_context
            [%string "unable to split: missing type variable %{tva}"])
  (* →I *)
  | Syntax.Lam ([ Syntax.PVar x ], e), TLam (ta, tb) -> (
      let%bind ctx' = check (Ctx.CEVar (x, ta) :: ctx) e tb in
      match Ctx.split (Ctx.var_named x) ctx' with
      | Some (_, _, ctx1) -> Env.return ctx1
      | None ->
          Env.non_wellformed_context
            [%string "unable to split: missing variable %{x}"])
  (* Sub *)
  | e, b ->
      let%bind a, ctx' = infer ctx e in
      subtype ctx' (Ctx.apply ctx' a) (Ctx.apply ctx' b)

(* Under input context Γ, e synthesizes output type A, with output context Δ *)
and infer ctx e =
  print_endline
    [%string "infer:\n  ctx=%{Ctx.show ctx}\n  e=%{Syntax.show_expr e}\n"];
  match e with
  (* 1I⇒ *)
  | Syntax.Unit -> Env.return (TUnit, ctx)
  (* Var *)
  | Syntax.Var x -> (
      match Ctx.find_map (Ctx.var_named x) ctx with
      | Some t -> Env.return (t, ctx)
      | None -> Env.non_wellformed_context [%string "unbound variable %{x}"])
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
        Ctx.CEVar (x, TExVar eva)
        :: Ctx.CEExVar evb :: Ctx.CEExVar eva :: Ctx.CEMarker eva :: ctx
      in
      let%bind ctx'' = check ctx' e (TExVar evb) in
      match Ctx.split (Ctx.marker_named eva) ctx'' with
      | Some (ctx2, _, ctx1) ->
          let mt = Ctx.apply ctx2 (TLam (TExVar eva, TExVar evb)) in
          let evs = Ctx.filter_map Ctx.exvar_unsolved ctx2 in
          let%bind evns = Env.fresh_tvars evs in
          if is_monotype mt then Env.return (gen_exvars mt evns, ctx1)
          else
            Env.no_rule_applicable
              [%string "infer: →I⇒' failed on non-monotype %{show_ty mt}"]
      | None ->
          Env.non_wellformed_context
            [%string
              "unable to split: missing marker for existential variable %{eva}"]
      )
  (* →E *)
  | Syntax.App (e1, [ e2 ]) ->
      let%bind a, ctx' = infer ctx e1 in
      infer_app ctx' (Ctx.apply ctx' a) e2
  | t ->
      Env.no_rule_applicable
        [%string "infer: no rule applicable for %{Syntax.show_expr t}"]

(* Under input context Γ, applying a function of type Ato e synthesizes type C, with output context Δ *)
and infer_app ctx ta e =
  print_endline
    [%string
      "infer_app:\n\
      \  ctx=%{Ctx.show ctx}\n\
      \  e=%{Syntax.show_expr e}\n\
      \  ta=%{show_ty ta}\n"];
  match ta with
  (* ∀App *)
  | TForall (tva, a) ->
      infer_app (Ctx.CEExVar tva :: ctx) (replace_tvar tva tva a) e
  (* α-hat App *)
  | TExVar eva -> (
      match Ctx.split (Ctx.exvar_unsolved_named eva) ctx with
      | Some (ctx2, _, ctx1) ->
          let%bind eva1 = Env.fresh_exvar in
          let%bind eva2 = Env.fresh_exvar in
          let ctx' =
            List.rev_append ctx2
              (Ctx.CESolved (eva, TLam (TExVar eva1, TExVar eva2))
              :: Ctx.CEExVar eva1 :: Ctx.CEExVar eva2 :: ctx1)
          in
          let%bind ctx'' = check ctx' e (TExVar eva1) in
          Env.return (TExVar eva2, ctx'')
      | None ->
          Env.non_wellformed_context
            [%string "unable to split: unbound existential variable %{eva}"])
  (* →App *)
  | TLam (a, c) ->
      let%bind ctx' = check ctx e a in
      Env.return (c, ctx')
  | t ->
      Env.no_rule_applicable
        [%string "infer_app: no rule applicable for %{show_ty t}"]

let e1 = Syntax.Lam ([ Syntax.PVar "x" ], Syntax.Var "x")

let e2 =
  Syntax.Lam
    ( [ Syntax.PVar "x" ],
      Syntax.Lam
        ([ Syntax.PVar "y" ], Syntax.App (Syntax.Var "x", [ Syntax.Var "y" ]))
    )

let t2 =
  TForall
    ( "a",
      TForall ("b", TLam (TLam (TLam (TVar "a", TVar "b"), TVar "a"), TVar "b"))
    )

(* let f x y = x y *)
