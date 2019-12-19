open Core

let read_camera () =
  let open Intcode.Let_syntax in
  let%map output = Intcode.read_string () in
  output
  |> String.strip
  |> String.split_lines
  |> Array.of_list_map ~f:String.to_array

let part1 file =
  let image = Intcode.eval (read_camera ()) (Intcode.load file) in
  let parameter_sum = ref 0 in
  let height = Array.length image in
  let width = Array.length image.(0) in
  for y = 1 to height - 2 do
    for x = 1 to width - 2 do
      let points =
        [ image.(y).(x)
        ; image.(y).(x+1)
        ; image.(y).(x-1)
        ; image.(y+1).(x)
        ; image.(y-1).(x) ]
      in
      if List.for_all points ~f:([%equal: char] '#') then
        parameter_sum := !parameter_sum + x*y;
    done;
  done;
  printf "%d\n" !parameter_sum
