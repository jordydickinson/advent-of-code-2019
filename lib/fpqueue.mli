open Core

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val enqueue : 'a t -> 'a -> int -> 'a t
val dequeue : 'a t -> ('a * 'a t) option
val dequeue_exn : 'a t -> 'a * 'a t
