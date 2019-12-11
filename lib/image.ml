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

let set_pixel image x y color =
  image.(y).(x) <- color

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
