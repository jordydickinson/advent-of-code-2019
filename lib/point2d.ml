open Core

include Tuple.Make (Int) (Int)
include Tuple.Comparable (Int) (Int)
include Tuple.Hashable (Int) (Int)
include Tuple.Sexpable (Int) (Int)

let taxi_norm (x, y) = abs x + abs y

let taxi_normalize (x, y) =
  let rec gcd n m =
    if Int.O.(m = 0)
    then n
    else gcd m (n%m)
  in
  let d = gcd (abs x) (abs y) in
  x/d, y/d

let angle (x, y) =
  Float.(atan2 (of_int y) (of_int x))
