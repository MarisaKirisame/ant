open Ant
module Words = Ant.Words
module Word = Ant.Word.Word
module TRC = TestRegCEK

let run () : unit =
  let memo = Ant.Memo.init_memo () in
  let memo_result = TRC.test_all memo (Memo.from_int 0) in
  let Memo.{ words = _; step; without_memo_step } = memo_result in
  Printf.printf "memo result: step=%d, without_memo_step=%d\n" step without_memo_step
