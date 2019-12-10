open Core

type position = Empty | Asteroid

module Bigvec2 = struct
  include Tuple.Make (Bignum) (Bignum)
  include Tuple.Comparable (Bignum) (Bignum)
  include Tuple.Hashable (Bignum) (Bignum)

  let norm (x, y) =
    Bignum.(abs x + abs y)

  let normalize (x, y) =
    let norm = norm (x, y) in
    Bignum.(x / norm, y / norm)

  let angle (x, y) =
    let angle = Float.atan2 (Bignum.to_float y) (Bignum.to_float x) in
    if Float.(angle < -pi / 2.0)
    then Float.(angle + pi*2.0)
    else angle
end

let read_asteroids file =
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
      |> List.map ~f:(fun x -> Bignum.(of_int x, of_int y))
  )
  |> Set.of_list (module Bigvec2)

let move_origin asteroid_map (x, y) =
  Set.remove asteroid_map (x, y)
  |> Set.map (module Bigvec2) ~f:(fun (x', y') -> Bignum.(x' - x, y' - y))

let count_visible_asteroids asteroid_map origin =
  let asteroid_map = move_origin asteroid_map origin in
  Set.map (module Bigvec2) asteroid_map ~f:Bigvec2.normalize
  |> Set.length

let vaporized_asteroids asteroids =
  let asteroids_map =
    Set.fold asteroids ~init:(Map.empty (module Bigvec2)) ~f:(
      fun map asteroid ->
        let normalized = Bigvec2.normalize asteroid in
        Map.update map normalized ~f:(function
          | None -> [asteroid]
          | Some asteroids -> asteroid :: asteroids
        )
    )
  in
  let directions =
    Map.keys asteroids_map
    |> List.sort ~compare:(
      fun a1 a2 ->
        Float.compare (Bigvec2.angle a1) (Bigvec2.angle a2)
    )
    |> Fqueue.of_list
  in
  let rec vaporized_asteroids' accum asteroids_map directions =
    match Fqueue.dequeue directions with
    | None -> List.rev accum
    | Some (direction, directions) ->
      let asteroids = Map.find_exn asteroids_map direction in
      match asteroids with
      | [] -> vaporized_asteroids' accum (Map.remove asteroids_map direction) directions
      | asteroid :: asteroids ->
        let accum = asteroid :: accum in
        let directions_map = Map.update asteroids_map direction ~f:(
          function
          | None -> failwith "Impossible."
          | Some _ -> asteroids
        )
        in
        let directions = Fqueue.enqueue directions direction in
        vaporized_asteroids' accum directions_map directions
  in
  vaporized_asteroids' [] asteroids_map directions

let part1 file =
  let asteroids = read_asteroids file in
  Set.map (module Int) asteroids ~f:(count_visible_asteroids asteroids)
  |> Set.max_elt_exn
  |> printf "%d\n"

let part2 file =
  let asteroids = read_asteroids file in
  let (x0, y0) =
    let count_visible_asteroids = Memo.general (count_visible_asteroids asteroids) in
    Set.fold asteroids ~init:Bignum.(zero, zero) ~f:(
      fun accum asteroid ->
        if count_visible_asteroids asteroid > count_visible_asteroids accum
        then asteroid
        else accum
    )
  in
  let asteroids = move_origin asteroids (x0, y0) in
  let vaporized = vaporized_asteroids asteroids in
  List.nth_exn vaporized 199
  |> fun (x, y) -> Bignum.((x + x0) * hundred + (y + y0))
  |> Bignum.to_string_hum
  |> printf "%s\n"
