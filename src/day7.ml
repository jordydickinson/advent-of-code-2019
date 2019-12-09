open Core

let part1 file =
  let machine = Intcode.load file in
  let settings = Util.permutations [0;1;2;3;4] in
  let try_setting machine setting =
    let rec try_setting' setting signal =
    match setting with
    | [] -> signal
    | phase :: setting ->
      let signal =
        Intcode.(
          exec machine
          |> send_exn ~input:phase
          |> send_exn ~input:signal
          |> returns_exn
        )
      in
      try_setting' setting signal
    in
    try_setting' setting 0
  in
  Sequence.map settings ~f:(try_setting machine)
  |> Sequence.fold ~init:0 ~f:max
  |> printf "%d\n"

let part2 file =
  let machine = Intcode.load file in
  let settings = Util.permutations [5;6;7;8;9] in
  let try_setting mem setting =
    let amps =
      List.map setting ~f:(
          fun phase ->
            Intcode.(exec mem |> send_exn ~input:phase)
        )
      |> Fqueue.of_list 
    in
    let rec try_setting' signal amps =
      match Fqueue.dequeue amps with
      | None -> signal
      | Some (amp, amps) ->
        let amp, signal =
          Intcode.(
            amp
            |> send_exn ~input:signal
            |> recv_exn
          )
        in
        if Intcode.is_halted amp
        then try_setting' signal amps
        else try_setting' signal (Fqueue.enqueue amps amp)
    in
    try_setting' 0 amps
  in
  Sequence.map settings ~f:(try_setting machine)
  |> Sequence.fold ~init:0 ~f:max
  |> printf "%d\n"
