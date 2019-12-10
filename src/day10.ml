open Core

type position = Empty | Asteroid

module Point = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
  include Tuple.Hashable (Int) (Int)
end

module Bigpoint = struct
  include Tuple.Make (Bignum) (Bignum)
  include Tuple.Comparable (Bignum) (Bignum)
  include Tuple.Hashable (Bignum) (Bignum)
end

let read_asteroid_map file =
  let parse_line line =
    line
    |> String.strip
    |> String.to_list
    |> List.filter_mapi ~f:(
      fun x c ->
        match c with
        | '.' -> None
        | '#' -> Some x
        | _ -> failwith "Invalid input"
    )
  in
  In_channel.input_lines file
  |> List.concat_mapi ~f:(
    fun y line ->
      parse_line line
      |> List.map ~f:(fun x -> x, y)
  )
  |> Set.of_list (module Point)

let count_visible_asteroids asteroid_map (x, y) =
  let slopes =
    Set.filter_map (module Bigpoint) asteroid_map ~f:(
      fun (x', y') ->
        if x = x' && y = y' then None else
        let dx, dy = x - x', y - y' in
        let norm = abs dx + abs dy in
        Some Bignum.O.(dx // norm, dy // norm)
    )
  in
  Set.length slopes

let part1 file =
  let asteroid_map = read_asteroid_map file in
  Set.map (module Int) asteroid_map ~f:(count_visible_asteroids asteroid_map)
  |> Set.max_elt_exn
  |> printf "%d\n"
