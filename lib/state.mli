open Core

include Monad_ext.S2

val get : ('s, 's) t
val set : 's -> (unit, 's) t
val run : ('a, 's) t -> 's -> 'a * 's
val eval : ('a, 's) t -> 's -> 'a
