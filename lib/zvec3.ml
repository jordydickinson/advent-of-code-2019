open Core

type t =
  { x : int
  ; y : int
  ; z : int
  }
[@@deriving equal, hash, fields]

let zero = { x = 0; y = 0; z = 0 }

let xhat = { zero with x = 1 }
let yhat = { zero with y = 1 }
let zhat = { zero with z = 1 }

let map v ~f =
  { x = f v.x
  ; y = f v.y
  ; z = f v.z
  }

let map2 u v ~f =
  { x = f u.x v.x
  ; y = f u.y v.y
  ; z = f u.z v.z
  }

let fold v ~init ~f = f (f (f init v.x) v.y) v.z
let fold' v ~f = f (f v.x v.y) v.z

let norm v = map v abs |> fold' ~f:(+)
let neg v = map v (Int.neg)
let add u v = map2 u v (+)
let sub u v = map2 u v (-)
let smul x v = map v (fun v' -> v' * x)
let dot u v = map2 u v ( * ) |> fold' ~f:(+)

let (+) = add
let (-) = sub
let (~-) = neg
let ( * ) = smul
let ( *+ ) = dot

let unitize v =
  let d = fold' v Util.gcd in
  map v (fun v' -> v' / d)

module O = struct
  let (~-) = (~-)
  let (+) = (+)
  let (-) = (-)
  let ( * ) = ( * )
  let ( *+ ) = ( *+ )
end
