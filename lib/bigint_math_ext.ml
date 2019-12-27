open Core

let egcd a b =
  let open Bigint.O in
  if a < Bigint.zero || b < Bigint.zero then failwith "egcd: arguments cannot be negative";
  let rec egcd a b =
    if a = Bigint.zero then abs b, Bigint.zero, Bigint.one else
    let gcd, r, q = egcd (Bigint.rem b a) a in
    gcd, q - b/a*r, r
  in
  egcd a b

let modinv d m =
  let open Bigint.O in
  let modinv d m =
    let gcd, d', _ = egcd d m in
    if gcd = Bigint.one
    then Some (d' % m)
    else None
  in
  modinv (d % m) m

let modpow =
  let open Bigint.O in
  let modpow b e m =
    let invalid_arg s = invalid_argf "modpow %s %s %s: %s"
      (Bigint.to_string b) (Bigint.to_string e) (Bigint.to_string m) s ()
    in
    if b < Bigint.zero then invalid_arg "base must be positive";
    if e < Bigint.zero then invalid_arg "exponent must be positive";
    if m < Bigint.zero then invalid_arg "modulus must be positive";
    if m = Bigint.one then Bigint.zero else
    let rec modpow' accum b e =
      if e <= Bigint.zero then accum else
      if e land Bigint.one = Bigint.one
      then modpow' (accum*b % m) (b*b % m) (e asr 1)
      else modpow' accum (b*b % m) (e asr 1)
    in
    modpow' Bigint.one (b % m) e
  in
  modpow
