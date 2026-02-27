let steps_file = "eval_steps_list_extend.json"

module Common = RunLiveCommon
module LC = Common.LC
open Core
open Yojson.Safe
open NamedExpr
module Ant = Ant

(* List of increasing-length prefixes of Common.random_list *)
let random_lists_exprs, _ =
  List.fold_right
    ~f:(fun n (res, (acc, list_size)) ->
      let new_acc = LC.ECons (LC.EInt n, acc) in
      ((new_acc, list_size + 1) :: res, (new_acc, list_size + 1)))
    (List.take Common.random_list 300) (* Using the whole list takes kinda long *)
    ~init:([], (LC.ENil, 0))

let random_lists_exprs = List.rev (List.take random_lists_exprs 100)

let write_steps_json oc (list_size : int) (r : Ant.Memo.exec_result) : unit =
  let json_of_profile entries = `List (List.map ~f:(fun (name, time) -> `List [ `String name; `Int time ]) entries) in
  let json =
    `Assoc
      [
        ("name", `String "exec_time");
        ("step", `Int r.step);
        ("list_size", `Int list_size);
        ("without_memo_step", `Int r.without_memo_step);
        ("memo_profile", Ant.Profile.dump_profile Ant.Profile.memo_profile |> json_of_profile);
        ("plain_profile", Ant.Profile.dump_profile Ant.Profile.plain_profile |> json_of_profile);
        ("cek_profile", Ant.Profile.dump_profile Ant.Profile.cek_profile |> json_of_profile);
      ]
  in
  Yojson.Safe.to_string json |> output_string oc;
  output_char oc '\n';
  flush oc

let run () =
  Common.with_outchannel steps_file (fun oc ->
      let write_steps = write_steps_json oc in
      Common.LC.populate_state ();
      let memo = Ant.Memo.init_memo () in
      let eval list_size expr = Common.eval_expression ~memo ~write_steps:(write_steps list_size) expr in
      print_endline "list_extend quicksort (quicksort expression fixed):";
      random_lists_exprs
      |> List.iteri ~f:(fun i (list_expr, list_size) ->
          let applied = LC.EApp (Common.quicksort_expr, list_expr) in
          Printf.printf "step %d value: %s\n" i (Common.value_to_string (eval list_size applied)));
      Common.write_memo_stats_json oc memo)
