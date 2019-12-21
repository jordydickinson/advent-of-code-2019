open Core

module Zvec2 = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
  include Tuple.Hashable (Int) (Int)
end

type orientation = Right | Left | Up | Down

let turn_left orientation =
  match orientation with
  | Right -> Up
  | Left -> Down
  | Up -> Left
  | Down -> Right

let turn_right orientation =
  match orientation with
  | Right -> Down
  | Left -> Up
  | Up -> Right
  | Down -> Left

let step_forward (x, y) orientation =
  match orientation with
  | Right -> x + 1, y
  | Left -> x - 1, y
  | Up -> x, y - 1
  | Down -> x, y + 1

let hull_to_image hull =
  let points = Map.key_set hull in
  let xs = Set.map (module Int) points (fun (x, _) -> x) in
  let ys = Set.map (module Int) points (fun (_, y) -> y) in
  let x_max, x_min =
    Set.max_elt_exn xs, Set.min_elt_exn xs
  in
  let y_max, y_min =
    Set.max_elt_exn ys, Set.min_elt_exn ys
  in
  let w, h = x_max - x_min + 1, y_max - y_min + 1 in
  let image = Image.create w h Image.Transparent in
  Set.iter points (
    fun (x, y) ->
      let color =
        match Map.find_exn hull (x, y) with
        | 0 -> Image.Black
        | 1 -> Image.White
        | _ -> failwith "Invalid hull color."
      in
      let x, y = x - x_min, y - y_min in
      Image.set_pixel image x y color
  );
  image

let run_hull_painter file hull =
  let rec robot_m hull pos orientation =
    let open State.Let_syntax in
    let color =
      match Map.find hull pos with
      | None -> 0 (* Black *)
      | Some color -> color
    in
    let%bind color = Intcode.write color >> Intcode.read in
    if Result.is_error color then return hull else
    let color = Result.ok color |> Option.value_exn in
    let%bind turn = Intcode.read in
    let turn = Result.ok turn |> Option.value_exn in
    let hull = Map.set hull pos color in
    let orientation =
      match turn with
      | 0 -> turn_left orientation
      | 1 -> turn_right orientation
      | _ -> failwithf "Invalid turn from robot: %d\n" turn ()
    in
    let pos = step_forward pos orientation in
    robot_m hull pos orientation
  in
  Intcode.eval (robot_m hull (0, 0) Up) (Intcode.load file)


let part1 file =
  let hull = run_hull_painter file (Map.empty (module Zvec2)) in
  printf "%d\n" @@ Map.length hull

let part2 file =
  let hull = Map.set (Map.empty (module Zvec2)) ~key:(0, 0) ~data:1 in
  let hull = run_hull_painter file hull in
  Image.print @@ hull_to_image hull
