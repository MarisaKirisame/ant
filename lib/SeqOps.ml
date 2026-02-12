open BatFingerTree

let cons_merge ~monoid ~measure ~merge (et : 'a) (seq : ('a, 'm) Generic.fg) : ('a, 'm) Generic.fg =
  if Generic.is_empty seq then Generic.singleton et
  else
    let tail, head = Generic.front_exn ~monoid ~measure seq in
    match merge et head with
    | Some merged -> Generic.cons ~monoid ~measure tail merged
    | None -> Generic.cons ~monoid ~measure seq et

let snoc_merge ~monoid ~measure ~merge (seq : ('a, 'm) Generic.fg) (et : 'a) : ('a, 'm) Generic.fg =
  if Generic.is_empty seq then Generic.singleton et
  else
    let head, tail = Generic.rear_exn ~monoid ~measure seq in
    match merge tail et with
    | Some merged -> Generic.snoc ~monoid ~measure head merged
    | None -> Generic.snoc ~monoid ~measure seq et

let append_merge ~monoid ~measure ~merge (x : ('a, 'm) Generic.fg) (y : ('a, 'm) Generic.fg) : ('a, 'm) Generic.fg =
  if Generic.is_empty x then y
  else if Generic.is_empty y then x
  else
    let xh, xt = Generic.rear_exn ~monoid ~measure x in
    let yt, yh = Generic.front_exn ~monoid ~measure y in
    match merge xt yh with
    | Some merged -> Generic.append ~monoid ~measure xh (Generic.cons ~monoid ~measure yt merged)
    | None -> Generic.append ~monoid ~measure x y
