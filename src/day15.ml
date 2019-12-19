open Core

let explore start machine =
  let move direction () =
    let open Intcode.Let_syntax in
    let input = match direction with
    | `North -> 1
    | `South -> 2
    | `West -> 3
    | `East -> 4 in
    match%map Intcode.(write input () >>= read) with
    | Ok 0 -> `Wall
    | Ok 1 -> `Floor
    | Ok 2 -> `Goal
    | Ok _ -> failwith "Unexpected machine output"
    | Error Blocking -> failwith "Machine expects input"
    | Error Halted -> failwith "Machine halted"
  in
  let rec explore pointmap = function
  | [] -> pointmap
  | (node, s) :: nodes ->
    let neighbors = Zvec2.
      [ node + yhat, `North
      ; node - yhat, `South
      ; node - xhat, `West
      ; node + xhat, `East
      ] |> List.filter ~f:(Fn.non @@ Fn.compose (Map.mem pointmap) fst)
    in
    let pointmap, nodes =
        List.fold neighbors ~init:(pointmap, nodes)
          ~f:(fun (pointmap, nodes) (node, direction) ->
                let node_type, s = Intcode.run (move direction ()) s in
                let pointmap = Map.set pointmap node node_type in
                match node_type with
                | `Wall ->
                  pointmap, nodes
                | _ ->
                  let nodes = (node, s) :: nodes in
                  pointmap, nodes)
    in
    explore pointmap nodes
  in
  let pointmap = Map.of_alist_exn (module Zvec2) [start, `Floor] in
  let nodes = [start, machine] in
  explore pointmap nodes

let find_goal pointmap =
  Map.to_alist pointmap
  |> List.fold_until ~init:Zvec2.zero
    ~f:(fun accum -> function
      | point, `Goal -> Stop point
      | _, `Floor -> Continue accum
      | _, `Wall -> Continue accum)
    ~finish:ident

let distances pointmap start =
  let rec distances explored = function
  | [] -> explored
  | (distance, node) :: frontier ->
    if not @@ Map.mem pointmap node then distances explored frontier else
    let explored = Map.set explored node distance in
    let neighbors = Zvec2.
      [ node + yhat
      ; node - yhat
      ; node - xhat
      ; node + xhat
      ] in
    let frontier = List.fold neighbors ~init:frontier
      ~f:(fun frontier neighbor ->
          if Map.mem explored neighbor then
            frontier
          else if phys_equal (Map.find_exn pointmap neighbor) `Wall then
            frontier
          else
            (distance + 1, neighbor) :: frontier) in
    distances explored frontier
  in
  distances (Map.empty (module Zvec2)) [0, start]

let part1 file =
  let pointmap = explore Zvec2.zero Intcode.(load file) in
  let goal = find_goal pointmap in
  let distances = distances pointmap goal in
  printf "%d\n" (Map.find_exn distances Zvec2.zero)

let part2 file =
  let pointmap = explore Zvec2.zero Intcode.(load file) in
  let goal = find_goal pointmap in
  let distances = distances pointmap goal in
  let max_distance = Map.data distances
    |> List.max_elt ~compare:compare
    |> Option.value_exn
  in
  printf "%d\n" max_distance
