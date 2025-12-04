let steps_file = "eval_steps_demand_driven.json"

let run () =
  RunLiveCommon.with_steps_writer steps_file (fun write_steps -> RunLiveCommon.run_demanded_benchmark ~write_steps ())
