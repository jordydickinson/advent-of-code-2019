open Core

let deal = Bigint.O.(-Bigint.one, -Bigint.one)
let deal_incr n = Bigint.O.(n, zero)
let cut n = Bigint.O.(Bigint.one, -n)

let parse_instruction line =
  if String.is_prefix line "cut " then
    cut (Bigint.of_string @@ String.chop_prefix_exn line "cut ")
  else if String.is_prefix line "deal with increment " then
    deal_incr (Bigint.of_string @@ String.chop_prefix_exn line "deal with increment ")
  else if String.equal line "deal into new stack" then
    deal
  else
    failwithf "Invalid instruction: %s" line ()

let parse_instructions s =
  let lines = String.strip s |> String.split_lines in
  List.fold lines ~init:[] ~f:begin fun instructions line ->
    let line = String.strip line in
    (parse_instruction line) :: instructions
  end
  |> List.rev

let input_perms file = parse_instructions (In_channel.input_all file)

let combine_perms instructions =
  let open Bigint.O in
  let rec combine (m', b') = function
    | [] -> m', b'
    | (m, b) :: instructions -> combine (m*m', (m*b' + b)) instructions
  in
  let m, b = combine (Bigint.one, Bigint.zero) instructions in
  m, b

let id_seq len = Array.init len ident

let permute_into dest src (m, b) =
  let len = Array.length src in
  if Array.length dest <> len then
    invalid_arg "dest, src, and perm length must be equal";
  let open Bigint.O in
  Array.iteri src (fun i n -> dest.(Bigint.to_int_exn ((m*(Bigint.of_int i) + b) % Bigint.of_int len)) <- n)

let permute src perm =
  let len = Array.length src in
  let dest = Array.create len 0 in
  permute_into dest src perm;
  dest

let permi_inv_times (m, b) ~len =
  let open Bigint.O in
  let%bind.Option m' = Bigint_math_ext.modinv m len in
  if m' = Bigint.one then None else
  let%map.Option d' = Bigint_math_ext.modinv (m'-Bigint.one) len in
  fun times ->
  let m'_pow = Bigint_math_ext.modpow m' times len in
  fun j ->
  ((j - b)*m'_pow - b*(m'_pow - Bigint.one)*d' + b) % len

let part1 file =
  let perm = input_perms file |> combine_perms in
  permute (id_seq 10007) perm
  |> Array.findi ~f:(fun _ -> (=) 2019)
  |> Option.value_exn
  |> fst
  |> printf "%d\n"

let part2 file =
  let times = Bigint.of_int 101_741_582_076_661 in
  let permlen = Bigint.of_int 119_315_717_514_047 in
  let perm = input_perms file |> combine_perms in
  let permi_inv_times = permi_inv_times perm ~len:permlen |> Option.value_exn in
  permi_inv_times times (Bigint.of_int 2020)
  |> Bigint.to_string
  |> printf "%s\n"
