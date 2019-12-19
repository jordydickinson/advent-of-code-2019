open Core

let oxygen_tank_distance start machine =
  let open Intcode.Let_syntax in
  let move direction () =
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
  let rec explore pointmap nodes =
    if Map.is_empty nodes then Error `NoPath else
    let distance, node, s, nodes =
      let distance, ns = Map.min_elt_exn nodes in
      match ns with
      | [] -> assert (false)
      | (node, s) :: [] ->
        let nodes = Map.remove nodes distance in
        distance, node, s, nodes
      | (node, s) :: ns ->
        let nodes = Map.set nodes distance ns in
        distance, node, s, nodes
    in
    if phys_equal (Map.find_exn pointmap node) `Goal then Ok distance else
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
                  let nodes = Map.update nodes (distance + 1)
                    ~f:(function
                        | None -> [node, s]
                        | Some nodes -> (node, s) :: nodes) in
                  pointmap, nodes)
    in
    explore pointmap nodes
  in
  let pointmap = Map.of_alist_exn (module Zvec2) [start, `Floor] in
  let nodes = Map.of_alist_exn (module Int) [0, [start, machine]] in
  explore pointmap nodes

let part1 file =
  match (oxygen_tank_distance Zvec2.zero Intcode.(load file)) with
  | Ok dist -> printf "%d\n" dist
  | Error `NoPath -> failwith "No path found"
