open Core

type machine
type error = Blocking | Halted

include Monad.S

val get : unit -> machine t

val getmem : int -> unit -> int t
val setmem : int -> int -> unit -> unit t

val load : In_channel.t -> machine
val run : 'a t -> machine -> 'a * machine
val eval : 'a t -> machine -> 'a

val write : int -> unit -> unit t
val write_all : int list -> unit -> unit t
val write_char : char -> unit -> unit t
val write_string : string -> unit -> unit t
val read : unit -> (int, error) result t
val read_char : unit -> (char, error) result t
val read_string : unit -> string t
val peek : unit -> (int, error) result t

val is_halted : unit -> bool t

val fold_read : init:('a t) -> f:('a -> int -> 'a t) -> unit -> 'a t
val collect : f:(int -> 'a) -> unit -> 'a list t
