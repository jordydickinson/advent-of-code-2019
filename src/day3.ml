open Core

module Step = struct
  type direction = Right | Left | Up | Down

  type t = 
    { direction : direction 
    ; offset : int }

  let make direction offset =
    { direction = direction; offset = offset }

  let of_string step_s =
    let offset = int_of_string @@ String.drop_prefix step_s 1 in
    match step_s.[0] with
    | 'R' -> make Right offset
    | 'L' -> make Left offset
    | 'U' -> make Up offset
    | 'D' -> make Down offset
    | _ -> failwith "Invalid step"
end

module Point = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
  include Tuple.Hashable (Int) (Int)

  let origin = (0, 0)

  let manhattan_dist p1 p2 =
    let x1, y1 = p1 in
    let x2, y2 = p2 in
    abs (x2 - x1) + abs (y2 - y1)
end

module PointList = struct
  type t = Point.t list

  let of_step start step =
    let rec of_step' accum start step =
      let x, y = start in
      if Step.(step.offset) = 0 then accum else
      let point =
        match step.direction with
        | Right -> x + 1, y
        | Left -> x - 1, y
        | Up -> x, y + 1
        | Down -> x, y - 1
      in
      of_step' (point :: accum) point {step with offset = step.offset - 1}
    in
    of_step' [] start step
end

module Grid = struct
  module PointMap = Map.Make (Point)
  type t = Int.Set.t PointMap.t

  let empty: t = PointMap.empty

  let rec trace_steps id grid start steps =
    let add_id set_opt =
      let set = Option.value ~default:Int.Set.empty set_opt in
      Some (Int.Set.add set id)
    in
    match steps with
    | [] -> grid
    | step :: steps ->
      let points = PointList.of_step start step in
      let start = List.hd_exn points in
      let grid = List.fold points ~init:grid ~f:(Map.change ~f:add_id) in
      trace_steps id grid start steps

  let intersections grid =
    Map.fold grid ~init:(Set.empty (module Point))
      ~f:(fun ~key ~data accum ->
            if Set.length data >= 2
            then Set.add accum key
            else accum)
end

let parse_line line =
  String.split line ~on:','
  |> List.map ~f:Step.of_string

let parse_file file =
  In_channel.input_lines file
  |> List.map ~f:parse_line

let part1 file =
  let grid =
    parse_file file
    |> List.foldi ~init:Grid.empty
        ~f:(fun id grid steps -> Grid.trace_steps id grid Point.origin steps)
  in
  let intersections = Grid.intersections grid in
  Set.to_list intersections
  |> List.map ~f:(Point.manhattan_dist Point.origin)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> string_of_int
