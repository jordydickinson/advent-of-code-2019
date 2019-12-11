open Core

module Point2d = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
  include Tuple.Hashable (Int) (Int)
end

type pixel =
  | Black
  | White
  | Transparent
[@@deriving equal]

type t = pixel array array

let create width height init =
  Array.make_matrix height width init

let copy image =
  Array.init (Array.length image) (
    fun y -> Array.copy image.(y)
  )

let width image = Array.length image.(0)

let height image = Array.length image

let to_array image = copy image

let to_array_nocopy image = image

let set_pixel image x y pixel =
  image.(y).(x) <- pixel

let draw_pixel image x y pixel =
  if not @@ [%equal: pixel] pixel Transparent
  then set_pixel image x y pixel

let draw_image dest src =
  Array.iteri src (
    fun y -> Array.iteri ~f:(
      fun x srcpixel -> draw_pixel dest x y srcpixel
    )
  )

let print image =
  Array.iter image (
    fun row ->
      Array.iter row (
        fun pixel ->
          if [%equal: pixel] pixel White
          then printf "█"
          else printf " "
      );
      printf "\n"
  )
