open Core

let part1 file =
  let machine = Intcode.load file in
  let values = Intcode.(
    exec machine
    |> send_exn ~input:1
    |> collect_exn
  )
  in
  List.iter values ~f:(printf "%d\n")

let part2 file =
  let machine = Intcode.load file in
  let values = Intcode.(
    exec machine
    |> send_exn ~input:2
    |> collect_exn
  )
  in
  List.iter values ~f:(printf "%d\n")
