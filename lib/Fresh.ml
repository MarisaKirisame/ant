let counter = ref 0

let next_fresh s =
  let i = !counter in
  counter := i + 1;
  s ^ string_of_int i
