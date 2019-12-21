open Core

let part1 file =
  let outputs = Intcode.(eval (write 1 >> collect ~f:ident) (load file)) in
  List.iter outputs ~f:(printf "%d\n")

let part2 file =
  let outputs = Intcode.(eval (write 5 >> collect ~f:ident) (load file)) in
  List.iter outputs ~f:(printf "%d\n")
