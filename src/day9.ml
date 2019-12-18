open Core

let part1 file =
  Intcode.(eval (write 1 () >>= collect ~f:ident) (load file))
  |> List.iter ~f:(printf "%d\n")

let part2 file =
  Intcode.(eval (write 2 () >>= collect ~f:ident) (load file))
  |> List.iter ~f:(printf "%d\n")
