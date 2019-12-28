open Core

let sd_and x y = Intcode.write_string @@ sprintf "AND %c %c\n" x y
let sd_or x y = Intcode.write_string @@ sprintf "OR %c %c\n" x y
let sd_not x y = Intcode.write_string @@ sprintf "NOT %c %c\n" x y

let sd_program1 =
  let open Intcode.Let_syntax in
  sd_not 'A' 'J' >>
  sd_not 'B' 'T' >>
  sd_or 'T' 'J' >>
  sd_not 'C' 'T' >>
  sd_or 'T' 'J' >>
  sd_and 'D' 'J'

let sd_program2 =
  let open Intcode.Let_syntax in
  sd_not 'A' 'J' >>
  sd_not 'B' 'T' >>
  sd_or 'T' 'J' >>
  sd_not 'C' 'T' >>
  sd_or 'T' 'J' >>
  sd_not 'J' 'T' >>
  sd_and 'J' 'T' >>
  sd_or 'E' 'T' >>
  sd_or 'H' 'T' >>
  sd_and 'D' 'T' >>
  sd_and 'T' 'J'

let sd_walk = Intcode.write_string "WALK\n"
let sd_run = Intcode.write_string "RUN\n"

let part1 file =
  Intcode.eval
    Intcode.(read_string >> sd_program1 >> sd_walk >> collect ~f:ident)
    (Intcode.load file)
  |> List.iter ~f:(printf "%d\n")

let part2 file =
  Intcode.eval
    Intcode.(read_string >> sd_program2 >> sd_run >> collect ~f:ident)
    (Intcode.load file)
  |> List.iter ~f:(printf "%d\n")
