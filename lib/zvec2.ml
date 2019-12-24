open Core

module T = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving compare, sexp, hash, fields]
end

include T
include Comparable.Make(T)

let make x y =
  { x = x
  ; y = y
  }

let zero = make 0 0
let xhat = make 1 0
let yhat = make 0 1

let map v ~f =
  { x = f v.x
  ; y = f v.y
  }

let map2 u v ~f =
  { x = f u.x v.x
  ; y = f u.y v.y
  }

let fold v ~init ~f = f (f init v.x) v.y
let fold' v ~f = f v.x v.y

let norm v = abs v.x + abs v.y
let neg v = map v (~-)
let add u v = map2 u v (+)
let sub u v = map2 u v (-)
let smul x v = map v (( * ) x)
let dot u v = map2 u v ( * ) |> fold' ~f:(+)

module Infix = struct
  let (~-) = neg
  let (+) = add
  let (-) = sub
  let ( * ) = smul
  let ( *+ ) = dot
end

include Infix

let dist u v = norm (u - v)

let angle v =
  Float.(atan2 (of_int v.x) (of_int v.y))

let unitize v =
  let d = fold' v ~f:Util.gcd in
  map v (fun vi -> vi/d)

module O = struct
  include Infix
  let zero = zero
  let xhat = xhat
  let yhat = yhat
end
