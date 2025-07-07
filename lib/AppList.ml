type 'a app_list_full = LeafAlist of 'a | AppAlist of 'a app_list_full * 'a app_list_full
type 'a app_list = 'a app_list_full option

let rec app_list_full_to_list_aux x aux = 
  match x with
  | (LeafAlist x) -> x :: aux
  | (AppAlist (x, y)) -> 
      let aux = app_list_full_to_list_aux y aux in
      app_list_full_to_list_aux x aux
let app_list_full_to_list x = app_list_full_to_list_aux x []
let app_list_full_to_list x =
  match x with
  | None -> []
  | Some x -> app_list_full_to_list x
let app_list_append x y =
  match (x, y) with
  | (None, _) -> y
  | (_, None) -> x
  | (Some x, Some y) -> Some (AppAlist (x, y))
let app_list_empty = None
let app_list_cons x xs = app_list_append (Some (LeafAlist x)) xs
let app_list_is_empty x =
  match x with
  | None -> true
  | Some _ -> false
let rec app_list_from_list x =
  match x with
  | [] -> app_list_empty
  | h :: t -> app_list_cons h (app_list_from_list t)