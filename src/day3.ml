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

  type t = int Int.Map.t PointMap.t

  let empty: t = PointMap.empty

  let trace_steps id grid steps =
    let add_len len map_opt =
      let map = Option.value ~default:Int.Map.empty map_opt in
      Some
        (Map.change map id ~f:(
          fun len_opt ->
            if Option.is_none len_opt
            then Some len
            else len_opt
        ))
    in
    let rec trace_steps' grid start steps len =
      match steps with
      | [] -> grid
      | step :: steps ->
        let points = PointList.of_step start step in
        let start = List.hd_exn points in
        let len = len + List.length points in
        let grid = List.foldi points ~init:grid
          ~f:(fun i -> Map.change ~f:(add_len (len - (i + 1))))
        in
        trace_steps' grid start steps len
    in
    trace_steps' grid Point.origin steps 1

  let intersections grid =
    Map.filter grid ~f:(fun traversals -> Map.length traversals >= 2)
end

let parse_line line =
  String.split line ~on:','
  |> List.map ~f:Step.of_string

let parse_file file =
  In_channel.input_lines file
  |> List.map ~f:parse_line

let trace_paths file =
  parse_file file
  |> List.foldi ~init:Grid.empty
      ~f:(fun id grid steps -> Grid.trace_steps id grid steps)

let part1 file =
  let grid = trace_paths file in
  let intersections = Grid.intersections grid in
  Map.keys intersections
  |> List.map ~f:(Point.manhattan_dist Point.origin)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> string_of_int

let part2 file =
  let grid = trace_paths file in
  let intersections = Grid.intersections grid in
  let sum_lengths traversals =
    Map.fold traversals ~init:0 ~f:(fun ~key ~data sum -> sum + data)
  in
  Map.map intersections ~f:sum_lengths
  |> Map.data
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> string_of_int
