open Core

type t =
  { x : int
  ; y : int
  }
[@@deriving compare, sexp, hash, fields][@@end]

include Comparable.S with type t := t

val make : int -> int -> t

val zero : t
val xhat : t
val yhat : t

val map : t -> f:(int -> int) -> t
val map2 : t -> t -> f:(int -> int -> int) -> t
val fold : t -> init:'accum -> f:('accum -> int -> 'accum) -> 'accum

(** A version of fold that doesn't take an `init` parameter. Equivalent to
  `f (x v) (y v)`. *)
val fold' : t -> f:(int -> int -> 'accum) -> 'accum

val norm : t -> int
val neg : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val smul : int -> t -> t
val dot : t -> t -> int

val (+) : t -> t -> t
val (-) : t -> t -> t
val (~-) : t -> t
val ( * ) : int -> t -> t
val ( *+ ) : t -> t -> int

val dist : t -> t -> int
val angle : t -> float
val unitize : t -> t

module O : sig
  val zero : t
  val xhat : t
  val yhat : t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val (~-) : t -> t
  val ( * ) : int -> t -> t
  val ( *+ ) : t -> t -> int
end
