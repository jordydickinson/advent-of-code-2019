open Core

(** `gcd n m` is the greatest common divisor of `n` and `m`. *)
val gcd : int -> int -> int

val factorial : int -> int

(** `permutations xs` is a lazily-generated sequence of all the permutations of
  `xs` in lexicographic order. The original ordering of `xs` is taken to be
  the lexicographically least element. *)
val permutations : 'a list -> 'a list Sequence.t
