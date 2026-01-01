open Ant
module Words = Ant.Words
module Word = Ant.Word.Word
module TRC = TestRegCEK

let run () : unit =
  let memo = Ant.Memo.init_memo () in
  let memo_result = TRC.nil memo (Memo.from_int 0) in
  let Memo.{ words = _; step; without_memo_step; wall_time; without_memo_wall_time } = memo_result in
  Printf.printf "memo result: step=%d, without_memo_step=%d, wall_time=%d, without_memo_wall_time=%d\n" step
    without_memo_step wall_time without_memo_wall_time
