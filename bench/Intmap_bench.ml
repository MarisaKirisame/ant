open Ant
open Core_bench

let bench_intmap () =
  let size = [ 32; 64; 128; 256; 512; 1024 ] in
  let args = [ 1; 10; 100; 1000; 10000 ] in
  let gen_seq n =
    let arr = Array.init n (fun x -> x) in
    Array.shuffle ~rand:Random.int arr;
    arr
  in
  Command_unix.run
    (Bench.make_command
       [
         Bench.Test.create_group ~name:"Create"
           [
             Bench.Test.create_indexed ~name:"Intmap.create" ~args:size
               (fun len -> Core.Staged.stage (fun () -> Intmap.create len));
             Bench.Test.create_indexed ~name:"Hashtbl.create" ~args:size
               (fun len -> Core.Staged.stage (fun () -> Hashtbl.create len));
           ];
         Bench.Test.create_group ~name:"Insert"
           [
             Bench.Test.create_indexed ~name:"Intmap.insert" ~args (fun len ->
                 let im = Intmap.create 32 in
                 let seq = gen_seq len in
                 Core.Staged.stage (fun () ->
                     for i = 0 to len - 1 do
                       Intmap.add im seq.(i) (i + 1)
                     done));
             Bench.Test.create_indexed ~name:"Hashtbl.insert" ~args (fun len ->
                 let tbl = Hashtbl.create 32 in
                 let seq = gen_seq len in
                 Core.Staged.stage (fun () ->
                     for i = 0 to len - 1 do
                       Hashtbl.add tbl seq.(i) (i + 1)
                     done));
           ];
         Bench.Test.create_group ~name:"Find"
           [
             Bench.Test.create_indexed ~name:"Intmap.find" ~args (fun len ->
                 let im = Intmap.create 32 in
                 for i = 0 to len - 1 do
                   Intmap.add im i (i + 1)
                 done;
                 let seq = gen_seq len in
                 Core.Staged.stage (fun () ->
                     for i = 0 to len - 1 do
                       ignore (Intmap.find im seq.(i))
                     done));
             Bench.Test.create_indexed ~name:"Hashtbl.find" ~args (fun len ->
                 let tbl = Hashtbl.create 32 in
                 for i = 0 to len - 1 do
                   Hashtbl.add tbl i (i + 1)
                 done;
                 let seq = gen_seq len in
                 Core.Staged.stage (fun () ->
                     for i = 0 to len - 1 do
                       ignore (Hashtbl.find tbl seq.(i))
                     done));
           ];
         Bench.Test.create_group ~name:"Erase"
           [
             Bench.Test.create_indexed ~name:"Intmap.erase" ~args (fun len ->
                 let im = Intmap.create 32 in
                 for i = 0 to len - 1 do
                   Intmap.add im i (i + 1)
                 done;
                 let seq = gen_seq len in
                 Core.Staged.stage (fun () ->
                     for i = 0 to len - 1 do
                       Intmap.remove im seq.(i)
                     done));
             Bench.Test.create_indexed ~name:"Hashtbl.erase" ~args (fun len ->
                 let tbl = Hashtbl.create 32 in
                 for i = 0 to len - 1 do
                   Hashtbl.add tbl i (i + 1)
                 done;
                 let seq = gen_seq len in
                 Core.Staged.stage (fun () ->
                     for i = 0 to len - 1 do
                       Hashtbl.remove tbl seq.(i)
                     done));
           ];
       ])
