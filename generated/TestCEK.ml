open Ant
open Word
open Memo
open Value
open Common
let memo = init_memo () 

let () = add_exp (fun w_0 -> ((assert_env_length w_0 1);match (resolve w_0 K) with | None -> ()| Some (hd_0, tl_0) -> (match (Word.get_value hd_0) with | c_0 when c_0 = tag_cont_done -> (exec_done w_0)| _ -> failwith "unreachable"))) 0
let () = Words.set_constructor_degree 0 (1)