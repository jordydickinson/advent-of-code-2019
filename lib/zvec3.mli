type t =
  { x : int
  ; y : int
  ; z : int
  }

val zero : t

val xhat : t
val yhat : t
val zhat : t

val map : t -> f:(int -> int) -> t
val map2 : t -> t -> f:(int -> int -> int) -> t
val fold : t -> f:(int -> int -> int) -> int

val norm : t -> int
val neg : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val smul : int -> t -> t
val dot : t -> t -> int

val unitize : t -> t

module O : sig
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val (~-) : t -> t
  val ( * ) : int -> t -> t
  val ( *+ ) : t -> t -> int
end
