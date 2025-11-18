open Syntax
open PPrint

module type Backend = sig
  val compile : 'a prog -> document
end
