(** `egcd n m` finds `gcd, n', m'` such that `gcd = gcd n m` and
  `n'*n + m'*m = gcd` by application of the extended Euclidean GCD algorithm. *)
val egcd : Bigint.t -> Bigint.t -> Bigint.t * Bigint.t * Bigint.t

(** `modinv d m` is `Some d'` if `d` has a modular inverse `d'` with respect to
  `m` and `None` otherwise. *)
val modinv : Bigint.t -> Bigint.t -> Bigint.t option

(** `modpow b e m` is `Int.pow b e % m`, taking care to keep intermediate
  results low. *)
val modpow : Bigint.t -> Bigint.t -> Bigint.t -> Bigint.t
