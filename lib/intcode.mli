open Core

type t
type state

val load : In_channel.t -> t
val exec : t -> state

val set : t -> int -> int -> unit

val send_exn : state -> input:int -> state
val recv : state -> state * int option
val recv_many : state -> state * int list
val recv_exn : state -> state * int
val is_halted : state -> bool
val expect_halted_exn : state -> unit
val returns_exn : state -> int
val collect_exn : state -> int list
