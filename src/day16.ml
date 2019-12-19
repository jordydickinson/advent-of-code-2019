open Core

let decode_message input start len =
  let output = Sequence.drop_eagerly input start |> Sequence.to_array in
  let outputlen = Array.length output in
  let inputlen = outputlen + start in
  let midpoint = Int.clamp_exn (inputlen / 2 - start) ~min:0 ~max:outputlen in
  let len = min len outputlen in
  let patterns =
    let base_pattern = Sequence.of_list [0; 1; 0; -1] in
    Array.init midpoint
      ~f:(fun i ->
        let pattern_i = Sequence.concat_map base_pattern ~f:(fun pattern_j ->
          Sequence.take (Sequence.repeat pattern_j) (start + i + 1))
          |> Sequence.repeat
          |> Sequence.concat in
        let pattern_i = Sequence.drop_eagerly pattern_i 1 in
        Sequence.take pattern_i inputlen
        |> Sequence.to_array)
  in
  let rec do_phases n =
    if n <= 0 then Array.slice output 0 len |> Array.to_list else begin
      for i = 0 to midpoint - 1 do
        for j = i + 1 to outputlen - 1 do
          output.(i) <- output.(i) + patterns.(i).(j) * output.(j)
        done;
        output.(i) <- abs output.(i) % 10
      done;
      for i = outputlen - 2 downto midpoint do
        output.(i) <- output.(i) + output.(i + 1)
      done;
      for i = outputlen - 2 downto midpoint do
        output.(i) <- abs output.(i) % 10
      done;
      do_phases (n - 1)
    end
  in
  do_phases 100

let input_signal file =
  In_channel.input_all file
  |> String.strip
  |> String.to_list
  |> Sequence.of_list
  |> Sequence.map ~f:(fun c -> int_of_char c - int_of_char '0')

let input_real_signal file =
  let signal = input_signal file |> Sequence.repeat in
  let signal = Sequence.take signal 10_000 |> Sequence.concat in
  let offset = Sequence.take signal 7
    |> Sequence.to_list
    |> List.fold ~init:0 ~f:(fun accum digit -> accum * 10 + digit) in
  signal, offset

let part1 file =
  let signal = input_signal file in
  let output = decode_message signal 0 8 in
  List.iter output ~f:(printf "%d");
  Out_channel.newline stdout

let part2 file =
  let signal, offset = input_real_signal file in
  let message = decode_message signal offset 8 in
  List.iter message ~f:(printf "%d");
  Out_channel.newline stdout
