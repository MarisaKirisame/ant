type profile = { slots : profile_slot Dynarray.t; mutable timer : timer_top option }
and profile_slot = { mutable total_time : int; name : string }
and profile_index = (profile * int) option
and timer_bot = { index : int }
and timer_top = { index : int; start_time : Timer.t; end_time : Timer.t; stack : timer_bot list }

let create_profile () : profile = { slots = Dynarray.create (); timer = None }

let register_slot (p : profile) (name : string) : profile_index =
  let index = Dynarray.length p.slots in
  Dynarray.add_last p.slots { total_time = 0; name };
  Some (p, index)

let with_slot (index : profile_index) (f : unit -> 'a) : 'a =
  let start_timer p index stack =
    let buf = Timer.create () in
    p.timer <- Some { index; start_time = buf; end_time = Timer.create (); stack };
    Timer.record buf
  in
  let stop_timer p timer =
    Timer.record timer.end_time;
    let elapsed_time = Int64.to_int @@ Timer.diff_nanoseconds_positive timer.start_time timer.end_time in
    let slot = Dynarray.get p.slots timer.index in
    slot.total_time <- slot.total_time + elapsed_time
  in
  match index with
  | None -> f ()
  | Some (p, i) ->
      (match p.timer with
      | None -> start_timer p i []
      | Some timer ->
          stop_timer p timer;
          start_timer p i ({ index = timer.index } :: timer.stack));
      let ret = f () in
      (match p.timer with
      | None -> failwith "unreachable"
      | Some timer -> (
          stop_timer p timer;
          assert (timer.index = i);
          match timer.stack with [] -> p.timer <- None | bot :: rest -> start_timer p bot.index rest));
      ret

let dump_profile (p : profile) : (string * int) list =
  assert (Option.is_none p.timer);
  let ret = Dynarray.to_list p.slots |> List.map (fun slot -> (slot.name, slot.total_time)) in
  Dynarray.iter (fun slot -> slot.total_time <- 0) p.slots;
  ret

let memo_profile = create_profile ()
let cek_profile = create_profile ()
let plain_profile = create_profile ()
