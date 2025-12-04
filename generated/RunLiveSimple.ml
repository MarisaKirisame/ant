let steps_file = "eval_steps_simple.json"

let run () =
  RunLiveCommon.with_steps_writer steps_file (fun write_steps -> RunLiveCommon.run_simple_benchmark ~write_steps ())
