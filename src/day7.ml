open Core

let part1 file =
  let mem = Intcode.load file in
  let settings = Util.permutations [0;1;2;3;4] in
  let try_setting mem setting =
    let rec try_setting' setting signal =
    match setting with
    | [] -> signal
    | phase :: setting ->
      let signal =
        Intcode.exec mem
        |> Intcode.send_exn ~input:phase
        |> Intcode.send_exn ~input:signal
        |> Intcode.returns_exn
      in
      try_setting' setting signal
    in
    try_setting' setting 0
  in
  Sequence.map settings ~f:(try_setting mem)
  |> Sequence.fold ~init:0 ~f:max
  |> printf "%d\n"
