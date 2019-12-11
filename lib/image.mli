type pixel = Black | White | Transparent
type t

val equal_pixel : pixel -> pixel -> bool

val create : int -> int -> pixel -> t
val copy : t -> t
val width : t -> int
val height : t -> int
val to_array : t -> pixel array array
val to_array_nocopy : t -> pixel array array
val set_pixel : t -> int -> int -> pixel -> unit
val draw_pixel : t -> int -> int -> pixel -> unit
val draw_image : t -> t -> unit
val print : t -> unit
