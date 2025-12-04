let steps_file = "eval_steps_left_to_right.json"

let run () =
  RunLiveCommon.with_steps_writer steps_file (fun write_steps ->
      RunLiveCommon.run_left_to_right_benchmark ~write_steps ())
