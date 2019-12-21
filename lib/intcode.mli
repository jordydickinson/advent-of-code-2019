open Core

type machine
type error = Blocking | Halted

type 'a t = ('a, machine) State.t

include Monad_ext.S with type 'a t := 'a t

val getmem : int -> int t
val setmem : int -> int  -> unit t

val load : In_channel.t -> machine
val run : 'a t -> machine -> 'a * machine
val eval : 'a t -> machine -> 'a

val write : int -> unit t
val write_all : int list -> unit t
val write_char : char -> unit t
val write_string : string -> unit t
val read : (int, error) result t
val read_char : (char, error) result t
val read_string : string t
val peek : (int, error) result t

val is_halted : bool t

val fold_read : init:('a t) -> f:('a -> int -> 'a t) -> 'a t
val collect : f:(int -> 'a) -> 'a list t
