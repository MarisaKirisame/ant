open Ant
open Core_bench

let bench_monoid_hash () =
  let args = [ 1; 10; 100; 1000; 10000 ] in
  Command_unix.run
    (Bench.make_command
       [
         Bench.Test.create_indexed ~name:"SL2" ~args (fun len ->
             let open Hash.SL2 in
             let acc = ref unit in
             Core.Staged.stage (fun () ->
                 for i = 0 to len - 1 do
                   acc.contents <- mul acc.contents (from_int i)
                 done));
         Bench.Test.create_indexed ~name:"SL2Slow" ~args (fun len ->
             let open Hash.SL2Slow in
             let acc = ref unit in
             Core.Staged.stage (fun () ->
                 for i = 0 to len - 1 do
                   acc.contents <- mul acc.contents (from_int i)
                 done));
         Bench.Test.create_indexed ~name:"MCRC32C" ~args (fun len ->
             let open Hash.MCRC32C in
             let acc = ref unit in
             Core.Staged.stage (fun () ->
                 for i = 0 to len - 1 do
                   acc.contents <- mul acc.contents (from_int i)
                 done));
         Bench.Test.create_indexed ~name:"DebugHash" ~args (fun len ->
             let open Hash.DebugHash in
             let acc = ref unit in
             Core.Staged.stage (fun () ->
                 for i = 0 to len - 1 do
                   acc.contents <- mul acc.contents (from_int i)
                 done));
       ])
