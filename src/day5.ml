open Core

let part1 file =
  let mem = Intcode.load file in
  let rec part1' state =
    let open Intcode in
    match state with
    | Receiving resume_with -> part1' @@ resume_with 1
    | Sending (i, resume) -> printf "%d\n" i; part1' @@ resume ()
    | Halted -> ()
  in
  part1' (Intcode.exec mem)

let part2 file =
  let mem = Intcode.load file in
  let rec part2' state =
    let open Intcode in
    match state with
    | Receiving resume_with -> part2' @@ resume_with 5
    | Sending (i, resume) -> printf "%d\n" i; part2' @@ resume ()
    | Halted -> ()
  in
  part2' (Intcode.exec mem)
