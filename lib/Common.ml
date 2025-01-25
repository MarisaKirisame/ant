exception EXN of string

let panic msg = raise (EXN msg)
let todo msg = panic ("todo: " ^ msg)
