open Core

module Starmap = struct
  type t = string String.Map.t

  let read_exn file =
    let read_line map line =
      let ids = String.split ~on:')' line in
      let parent = List.hd_exn ids in
      let child = List.hd_exn @@ List.tl_exn ids in
      Map.add_exn map child parent
    in
    In_channel.fold_lines file ~init:String.Map.empty ~f:read_line

  let fold_up (starmap : t) ~start ~init ~f =
    let rec fold_up' accum node =
      let accum = f accum node in
      match Map.find starmap node with
      | None -> accum
      | Some parent ->
        fold_up' accum parent
    in
    fold_up' init start

  let distance starmap ~node1 ~node2 =
    let path node =
      fold_up starmap ~start:node ~init:[] ~f:(fun path node -> node :: path)
    in
    let path1 = path node1 in
    let path2 = path node2 in
    let prefixlen =
      let pathzip, _ = List.zip_with_remainder path1 path2 in
      let noprefix = List.drop_while pathzip ~f:(fun (a, b) -> [%equal: string] a b) in
      List.length pathzip - List.length noprefix
    in
    let path1 = List.drop path1 prefixlen in
    let path2 = List.drop path2 prefixlen in
    List.length path1 + List.length path2 - 2
  
  let compute_checksum starmap =
    let memo = String.Map.empty in
    let rec compute_checksum' node memo return =
      match Map.find memo node with
      | Some value -> return memo value
      | None ->
        let return memo value =
          let memo = Map.add_exn memo node value in
          return memo value
        in
        match Map.find starmap node with
        | None -> return memo 0
        | Some parent ->
          let return memo value = return memo (value + 1) in
          compute_checksum' parent memo return 
    in
    let nodes = Map.keys starmap in
    List.fold nodes ~init:(0, memo)
      ~f:(
        fun (accum, memo) node ->
          let return memo value = accum + value, memo in
          compute_checksum' node memo return
      )
    |> fst
end

let part1 file =
  let starmap = Starmap.read_exn file in
  let checksum = Starmap.compute_checksum starmap in
  printf "%d\n" checksum

let part2 file =
  let starmap = Starmap.read_exn file in
  let distance = Starmap.distance starmap "YOU" "SAN" in
  printf "%d\n" distance
