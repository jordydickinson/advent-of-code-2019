open Core

type 'a t = 'a Fqueue.t Int.Map.t

let empty = Int.Map.empty

let is_empty = Map.is_empty

let enqueue q elt pr =
  Map.update q pr (function
  | None -> Fqueue.enqueue Fqueue.empty elt
  | Some q -> Fqueue.enqueue q elt)

let dequeue q =
  let%bind.Option pr, eltq = Map.min_elt q in
  let%bind.Option elt, eltq = Fqueue.dequeue eltq in
  let q =
    if Fqueue.is_empty eltq
    then Map.remove q pr
    else Map.set q pr eltq
  in
  Some (elt, q)

let dequeue_exn q = Option.value_exn (dequeue q)
