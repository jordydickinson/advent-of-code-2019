open Core

type t = Image.t list

val input : In_channel.t -> width:int -> height:int -> t
val to_image : t -> Image.t
val to_layers : t -> Image.t list
val to_layers_nocopy : t -> Image.t list
