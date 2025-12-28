type profile_slot = { mutable total_time : int; name : string }
type profile_index = int option
type timer_bot = { index : int }
type timer_top = { index : int; start_time : Time_stamp_counter.t; stack : timer_bot list }
type profile = { slots : profile_slot Dynarray.t; mutable timer : timer_top option }

let create_profile () : profile = { slots = Dynarray.create (); timer = None }

let register_slot (p : profile) (name : string) : profile_index =
  let index = Dynarray.length p.slots in
  Dynarray.add_last p.slots { total_time = 0; name };
  Some index

let with_slot (p : profile) (index : profile_index) (f : unit -> 'a) : 'a =
  let start_timer index stack =
    let start_time = Time_stamp_counter.now () in
    p.timer <- Some { index; start_time; stack }
  in
  let stop_timer timer =
    let end_time = Time_stamp_counter.now () in
    let calibrator = Lazy.force Time_stamp_counter.calibrator in
    let elapsed_time =
      Time_stamp_counter.diff end_time timer.start_time
      |> Time_stamp_counter.Span.to_time_ns_span ~calibrator
      |> Core.Time_ns.Span.to_int63_ns |> Core.Int63.to_int_exn
    in
    let slot = Dynarray.get p.slots timer.index in
    slot.total_time <- slot.total_time + elapsed_time
  in
  match index with
  | None -> f ()
  | Some i ->
      (match p.timer with
      | None -> start_timer i []
      | Some timer ->
          stop_timer timer;
          start_timer i ({ index = timer.index } :: timer.stack));
      let ret = f () in
      (match p.timer with
      | None -> failwith "unreachable"
      | Some timer -> (
          stop_timer timer;
          assert (timer.index = i);
          match timer.stack with [] -> p.timer <- None | bot :: rest -> start_timer bot.index rest));
      ret

let dump_profile (p : profile) : (string * int) list =
  assert (Option.is_none p.timer);
  let ret = Dynarray.to_list p.slots |> List.map (fun slot -> (slot.name, slot.total_time)) in
  Dynarray.iter (fun slot -> slot.total_time <- 0) p.slots;
  ret

let memo_profile = create_profile ()
let plain_profile = create_profile ()
