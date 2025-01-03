open Syntax
open Fresh

let cps (expr : expr) =
  let mk_fresh_param_k () =
    let k = next_fresh "__k" in
    (PVar k, Var k)
  in
  let is_atomic = function
    | Unit | Int _ | Float _ | Bool _ | Str _ | Builtin _ | Var _ | Ctor _
    | Lam _ ->
        true
    | _ -> false
  in
  let ( let* ) e f = e f in
  let rec atom = function
    | Unit -> Unit
    | Int i -> Int i
    | Float f -> Float f
    | Bool b -> Bool b
    | Str s -> Str s
    | Builtin b -> Builtin b
    | Var x -> Var x
    | Ctor x -> Ctor x
    | Lam (xs, e) ->
        let pk, vk = mk_fresh_param_k () in
        Lam (xs @ [ pk ], cps'' e vk)
    | _ -> failwith "not an atom"
  and cps' e k =
    match e with
    | x when is_atomic x -> k (atom x)
    | App (f, xs) ->
        assert (List.length xs <= 1);
        let* f = cps' f in
        let* xs = cps_l' xs in
        let pk, vk = mk_fresh_param_k () in
        App (f, xs @ [ Lam ([ pk ], k vk) ])
    | Op (op, e1, e2) ->
        let* e1 = cps' e1 in
        let* e2 = cps' e2 in
        k (Op (op, e1, e2))
    | Tup es ->
        let* es = cps_l' es in
        k (Tup es)
    | Arr es ->
        let* es = cps_l' es in
        k (Arr es)
    | Let (BOne (x, e1), e2) ->
        let* e1 = cps' e1 in
        let* e2 = cps' e2 in
        k (Let (BOne (x, e1), e2))
    | Let (BRec xs, e2) ->
        let rhs = List.map snd xs in
        let* rhs = cps_l' rhs in
        let* e2 = cps' e2 in
        let r = Let (BRec (List.combine (List.map fst xs) rhs), e2) in
        k r
    | Sel (e, x) ->
        let* e = cps' e in
        k (Sel (e, x))
    | If (e1, e2, e3) ->
        let pk, vk = mk_fresh_param_k () in
        let pk2, vk2 = mk_fresh_param_k () in
        let* e1 = cps' e1 in
        App
          ( Lam ([ pk ], If (e1, cps'' e2 vk, cps'' e3 vk)),
            [ Lam ([ pk2 ], k vk2) ] )
    | Match (_, MatchPattern _) -> failwith "not implemented"
    | _ -> failwith "not an valid expr"
  and cps_l' es k =
    match es with
    | [] -> k []
    | e :: es' -> cps' e (fun e' -> cps_l' es' (fun es' -> k (e' :: es')))
  and cps'' e cont =
    match e with
    | x when is_atomic x -> App (cont, [ atom x ])
    | App (f, xs) ->
        assert (List.length xs <= 1);
        let* f = cps' f in
        let* xs = cps_l' xs in
        App (f, xs @ [ cont ])
    | Op (op, e1, e2) ->
        let* e1 = cps' e1 in
        let* e2 = cps' e2 in
        App (cont, [ Op (op, e1, e2) ])
    | Tup es ->
        let* es = cps_l' es in
        App (cont, [ Tup es ])
    | Arr es ->
        let* es = cps_l' es in
        App (cont, [ Arr es ])
    | Let (BOne (x, e1), e2) ->
        let* e1 = cps' e1 in
        let* e2 = cps' e2 in
        App (cont, [ Let (BOne (x, e1), e2) ])
    | Let (BRec xs, e2) ->
        let rhs = List.map snd xs in
        let* rhs = cps_l' rhs in
        let* e2 = cps' e2 in
        let r = Let (BRec (List.combine (List.map fst xs) rhs), e2) in
        App (cont, [ r ])
    | Sel (e, x) ->
        let* e = cps' e in
        App (cont, [ Sel (e, x) ])
    | If (e1, e2, e3) ->
        let* e1 = cps' e1 in
        If (e1, cps'' e2 cont, cps'' e3 cont)
    | Match (_, MatchPattern _) -> failwith "not implemented"
    | _ -> failwith "not an valid expr"
  in
  cps' expr (fun x -> x)

let cps_prog (prog : prog) =
  List.map (function Type _ as x -> x | Term (p, e) -> Term (p, cps e)) prog
