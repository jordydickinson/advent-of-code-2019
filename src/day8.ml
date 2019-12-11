open Core

let part1 file =
  let sif = Sif.input file 25 6 in
  let count_pixels layer pixel =
    Array.fold layer ~init:0 ~f:(
      fun accum row -> accum + Array.count row ~f:([%equal: Image.pixel] pixel)
    )
  in
  let layer =
    Sif.to_layers_nocopy sif
    |> List.map ~f:Image.to_array_nocopy
    |> List.min_elt ~compare:(
      fun layer1 layer2 ->
        let blacks1 = count_pixels layer1 Image.Black in
        let blacks2 = count_pixels layer2 Image.Black in
        compare blacks1 blacks2
    )
    |> Option.value_exn
  in
  let whites = count_pixels layer Image.White in
  let transparents = count_pixels layer Image.Transparent in
  printf "%d\n" (whites*transparents)

let part2 file =
  Sif.input file 25 6
  |> Sif.to_image
  |> Image.print
