open Core

let part1 file =
  let program = Intcode.load file in
  let settings = Util.permutations [0;1;2;3;4] in
  let try_setting program setting =
    List.fold setting ~init:0
      ~f:(fun signal phase ->
        let signal = Intcode.(
          eval (write_all [phase; signal] () >>= read) program)
        in
        signal |> Result.ok |> Option.value_exn
      )
  in
  Sequence.map settings ~f:(try_setting program)
  |> Sequence.fold ~init:0 ~f:max
  |> printf "%d\n"

let part2 file =
  let program = Intcode.load file in
  let settings = Util.permutations [5;6;7;8;9] in
  let try_setting program setting =
    let amps = List.map setting ~f:(fun phase ->
        let (), amp = Intcode.(run (write phase ()) program) in amp)
      |> Fqueue.of_list in
    let rec try_setting' signal amps =
      match Fqueue.dequeue amps with
      | None -> signal
      | Some (amp, amps) ->
        let maybe_signal, amp = Intcode.(run (write signal () >>= read) amp) in
        match maybe_signal with
        | Error Intcode.Halted -> try_setting' signal amps
        | Error Intcode.Blocking -> failwith "Machine shouldn't block"
        | Ok signal -> try_setting' signal (Fqueue.enqueue amps amp)
    in
    try_setting' 0 amps
  in
  Sequence.map settings ~f:(try_setting program)
  |> Sequence.fold ~init:0 ~f:max
  |> printf "%d\n"
