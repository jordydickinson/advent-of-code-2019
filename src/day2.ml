open Core

let parse_program file =
  In_channel.input_all file
  |> String.strip
  |> String.split ~on:','
  |> Array.of_list_map ~f:int_of_string

let run_program memory arg1 arg2 =
  let memory = Array.copy memory in
  memory.(1) <- arg1;
  memory.(2) <- arg2;
  let rec run_program' i =
    let next () = run_program' (i + 4) in
    let opcode = memory.(i) in
    let in1 = memory.(memory.(i+1)) in
    let in2 = memory.(memory.(i+2)) in
    let outp = memory.(i+3) in
    match opcode with
    | 1 -> (* add *)
      memory.(outp) <- in1 + in2;
      next ()
    | 2 -> (* mul *)
      memory.(outp) <- in1 * in2;
      next ()
    | 99 -> memory.(0) (* halt *)
    | _ -> failwith "Unknown error encountered while running program"
  in
  run_program' 0

let part1 file =
  let memory = parse_program file in
  run_program memory 12 2
  |> printf "%d\n"

let part2 file =
  let memory = parse_program file in
  let rec part2' noun verb =
    if run_program memory noun verb = 19690720 then
      100 * noun + verb
    else if verb = 99 then
      part2' (noun + 1) 0
    else
      part2' noun (verb + 1)
  in
  part2' 0 0 
  |> printf "%d\n"
