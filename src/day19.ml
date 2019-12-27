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
      scan_area
        (Map.set accum (Zvec2.make x y) (State.eval (scan_point x y) machine))
        (x + 1)
        y
    end
  in
  scan_area Zvec2.Map.empty 0 0

let print_area pointmap =
  let rec print_area x y =
    match Map.find pointmap (Zvec2.make x y) with
    | None when x = 0 -> ()
    | None -> printf "\n"; print_area 0 (y + 1)
    | Some `Stationary -> printf "."; print_area (x + 1) y
    | Some `Pulled -> printf "#"; print_area (x + 1) y
  in
  print_area 0 0

let find_closest_square machine sidelen =
  let rec find_closest_square x y =
    match Intcode.eval (scan_point x y) machine with
    | `Stationary -> find_closest_square x (y + 1)
    | `Pulled ->
      match Intcode.eval (scan_point (x - sidelen + 1) (y + sidelen - 1)) machine with
      | `Stationary -> find_closest_square (x + 1) y
      | `Pulled -> x - sidelen + 1, y
  in
  find_closest_square (sidelen - 1) 0

let part1 file =
  let scan = (scan_area (Intcode.load file) 50 50) in
  List.count (Map.data scan) (function
  | `Stationary -> false
  | `Pulled -> true)
  |> printf "%d\n"

let part2 file =
  let x, y = find_closest_square (Intcode.load file) 100 in
  printf "%d\n" (x*10_000 + y)
