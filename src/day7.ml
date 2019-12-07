open Core

let part1 file =
  let mem = Intcode.load file in
  let settings = Util.permutations [0;1;2;3;4] in
  let input_exn state ~input =
    match state with
    | Intcode.Input (resume_with) -> resume_with input
    | _ -> failwith "Expected input."
  in
  let output_exn state ~f =
    match state with
    | Intcode.Output (value, resume) -> f value resume
    | _ -> failwith "Expected output."
  in
  let halt_exn state =
    match state with
    | Intcode.Halt -> ()
    | _ -> failwith "Expected halt."
  in
  let try_setting mem setting =
    let rec try_setting' setting signal =
    match setting with
    | [] -> signal
    | i :: setting ->
      let signal =
        Intcode.exec mem
        |> input_exn ~input:i
        |> input_exn ~input:signal
        |> output_exn ~f:(fun signal resume -> halt_exn (resume ()); signal)
      in
      try_setting' setting signal
    in
    try_setting' setting 0
  in
  Sequence.map settings ~f:(try_setting mem)
  |> Sequence.fold ~init:0 ~f:max
  |> printf "%d\n"
