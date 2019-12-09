open Core

let part1 file =
  Intcode.(
    load file
    |> exec
    |> send_exn ~input:1
    |> collect_exn
    |> List.iter ~f:(printf "%d\n")
  )

let part2 file =
  Intcode.(
    load file
    |> exec
    |> send_exn ~input:5
    |> returns_exn
    |> printf "%d\n"
  )
