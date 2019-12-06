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
