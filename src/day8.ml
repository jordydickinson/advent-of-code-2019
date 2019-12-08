open Core

let read_layers (width, height) file =
  let chars =
    In_channel.input_all file
    |> String.strip
    |> String.to_list
  in
  let read_layer chars =
    let layer_cs, chars = List.split_n chars (width*height) in
    let layer =
      Array.of_list_map layer_cs ~f:(fun c -> int_of_char c - int_of_char '0')
    in
    layer, chars
  in
  let rec read_layers' layers chars =
    if List.is_empty chars then List.rev layers else
    let layer, chars = read_layer chars in
    read_layers' (layer :: layers) chars
  in
  read_layers' [] chars

let merge_layers layer1 layer2 =
  Array.map2_exn layer1 layer2 ~f:(
    fun pixel1 pixel2 ->
      if pixel1 = 2
      then pixel2
      else pixel1
  )

let show_image (width, height) image =
  Array.iteri image ~f:(
    fun i pixel ->
      if i <> 0 && i % width = 0 then printf "\n";
      if pixel = 1
      then printf "█"
      else printf " "
  );
  printf "\n"

let part1 file =
  let layers = read_layers (25, 6) file in
  List.min_elt layers ~compare:(
    fun layer1 layer2 ->
      let zeros1 = Array.count layer1 ~f:((=) 0) in
      let zeros2 = Array.count layer2 ~f:((=) 0) in
      compare zeros1 zeros2
  )
  |> Option.value_exn
  |> Array.fold ~init:(0, 0) ~f:(
    fun (ones, twos) pixel ->
      if pixel = 1
      then ones + 1, twos
      else if pixel = 2
      then ones, twos + 1
      else ones, twos
  )
  |> Tuple2.uncurry ( * )
  |> printf "%d\n"

let part2 file =
  let layers = read_layers (25, 6) file in
  List.fold layers ~init:(Array.create (25*6) 2) ~f:merge_layers
  |> show_image (25, 6)
