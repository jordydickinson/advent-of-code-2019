open Core

let scan_point x y =
  let open Intcode.Let_syntax in
  match%map Intcode.(write x >> write y >> read) with
  | Ok 0 -> `Stationary
  | Ok 1 -> `Pulled
  | Ok _ -> failwith "Invalid robot output"
  | Error Blocking -> failwith "Robot blocking"
  | Error Halted -> failwith "Robot halted"

let scan_area machine w h =
  let rec scan_area accum x y =
    if y >= h then accum else
    if x >= w then scan_area accum 0 (y + 1) else begin
      printf "Scanning <%d, %d>\n" x y;
      scan_area
        (Map.set accum (Zvec2.make x y) (State.eval (scan_point x y) machine))
        (x + 1)
        y
    end
  in
  scan_area Zvec2.Map.empty 0 0

let part1 file =
  let scan = (scan_area (Intcode.load file) 50 50) in
  List.count (Map.data scan) (function
  | `Stationary -> false
  | `Pulled -> true)
  |> printf "%d\n"
