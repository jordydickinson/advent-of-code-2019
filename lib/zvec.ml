open Core

type t = int array

let dim v = Array.length v

let check_dim u v =
  if dim u <> dim v
  then failwithf "Dimensions mismatch: %d and %d" (dim u) (dim v) ()

let zero dim =
  Array.create dim 0

let add u v =
  check_dim u v;
  Array.map2_exn u v ~f:(+)

let add_inplace u v =
  check_dim u v;
  Array.iteri u ~f:(
    fun i ui ->
      u.(i) <- ui + v.(i)
  )

let norm v = Array.fold v ~init:0 ~f:(fun accum x -> accum + abs x)

let unitize v =
  let rec gcd n m =
    if m = 0
    then n
    else gcd m (n%m)
  in
  let d = Array.fold v ~init:0 ~f:gcd in
  Array.map v ~f:(fun x -> x/d)
