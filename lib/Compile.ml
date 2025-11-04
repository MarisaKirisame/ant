open Syntax
open PPrint

module type Backend = sig
  val compile : stmt list -> document
end

module Memo = struct
  let compile = CompileMemo.pp_cek_ant
end

module Seq = struct
  let compile = CompileSeq.compile_ant
end
