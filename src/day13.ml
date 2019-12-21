open Core

type tile =
  | Empty
  | Wall
  | Block
  | Paddle
  | Ball

let parse_tile_id = function
  | 0 -> Empty
  | 1 -> Wall
  | 2 -> Block
  | 3 -> Paddle
  | 4 -> Ball
  | _ -> failwith "Bad tile ID"

let parse_outputs outs =
  let rec parse_outputs' score tiles ball paddle outs =
    match outs with
    | [] -> score, tiles, ball, paddle
    | -1 :: 0 :: score :: outs ->
      parse_outputs' score tiles ball paddle outs
    | x :: y :: tile_id :: outs ->
      let tile = parse_tile_id tile_id in
      let tiles = tile :: tiles in
      begin match tile with 
      | Ball -> parse_outputs' score tiles (x, y) paddle outs
      | Paddle -> parse_outputs' score tiles ball (x, y) outs
      | _ -> parse_outputs' score tiles ball paddle outs
      end
    | _ -> failwith "Unexpected number of outputs"
  in
  parse_outputs' 0 [] (0, 0) (0, 0) outs

let part1 file =
  let _, tiles, _, _ = parse_outputs Intcode.(eval (collect ~f:ident) (load file)) in
  List.count tiles ~f:(phys_equal Block)
  |> printf "%d\n"

let part2 file =
  let play_game =
    let open State.Let_syntax in
    let rec play_game' score =
      let%bind outputs = Intcode.(collect ~f:ident) in
      let score, _, (ball_x, _), (paddle_x, _) = parse_outputs outputs in
      if%bind Intcode.is_halted then return score else
      Intcode.write (compare ball_x paddle_x) >>
      play_game' score
    in
    Intcode.(setmem 0 2) >> play_game' 0
  in
  let score = Intcode.(eval play_game (load file)) in
  printf "%d\n" score
