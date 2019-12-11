open Core

type t = Image.t list

let input file ~width ~height =
  let rec input' accum image_cs =
    let rec parse_layer image layer_cs y =
      if List.is_empty layer_cs then () else
      let row, layer_cs = List.split_n layer_cs width in
      List.iteri row (
        fun x pixel_c ->
          let pixel =
            match pixel_c with
            | '0' -> Image.Black
            | '1' -> White
            | '2' -> Transparent
            | _ -> failwithf "Invalid pixel value in SIF image: %c" pixel_c ()
          in
          Image.draw_pixel image x y pixel
      );
      parse_layer image layer_cs (y + 1)
    in
    if List.is_empty image_cs then accum else
    let layer_cs, image_cs = List.split_n image_cs (width*height) in
    let layer = Image.create width height Image.Transparent in
    parse_layer layer layer_cs 0;
    input' (layer :: accum) image_cs
  in
  let image_cs =
    In_channel.input_all file
    |> String.strip
    |> String.to_list
  in
  input' [] image_cs

let to_image sif =
  let image = Image.copy @@ List.hd_exn sif in
  List.iter sif (Image.draw_image image);
  image
