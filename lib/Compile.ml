open Syntax
open PPrint

module type Backend = sig
  val compile : stmt list -> document
end
