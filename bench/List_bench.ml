open Core_bench

let bench_list () =
  let args = [ 1; 10; 100; 1000; 10000; 100000; 1000000 ] in
  Command_unix.run
    (Bench.make_command
       [
         Bench.Test.create_indexed ~name:"Map" ~args (fun len ->
             Core.Staged.stage (fun () -> List.map (fun i -> i + 1) (List.init len (fun i -> i))));
       ])
