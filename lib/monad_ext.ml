open Core

(* Note: No _indexed or Ident. *)

module type Basic = Monad.Basic
module type Basic2 = Monad.Basic2
module type Basic3 = Monad.Basic3

module type Infix = sig
  type 'a t
  include Monad.Infix with type 'a t := 'a t
  val (>>) : 'a t -> 'b t -> 'b t
end

module type Infix2 = sig
  type ('a, 'e) t
  include Monad.Infix2 with type ('a, 'e) t := ('a, 'e) t
  val (>>) : ('a, 'e) t -> ('b, 'e) t -> ('b, 'e) t
end

module type Infix3 = sig
  type ('a, 'd, 'e) t
  include Monad.Infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t
  val (>>) : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> ('b, 'd, 'e) t
end

module type Syntax = sig
  type 'a t
  module Let_syntax : sig
    val return : 'a -> 'a t
    include Infix with type 'a t := 'a t
    module Let_syntax : sig
      val return : 'a -> 'a t
      val bind : 'a t -> f:('a -> 'b t) -> 'b t
      val map : 'a t -> f:('a -> 'b) -> 'b t
      val both : 'a t -> 'b t -> ('a * 'b) t
      module Open_on_rhs : sig end
    end
  end
end

module type Syntax2 = sig
  type ('a, 'e) t
  module Let_syntax : sig
    val return : 'a -> ('a, 'e) t
    include Infix2 with type ('a, 'e) t := ('a, 'e) t
    module Let_syntax : sig
      val return : 'a -> ('a, _) t
      val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
      val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
      val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
      module Open_on_rhs : sig end
    end
  end
end

module type Syntax3 = sig
  type ('a, 'd, 'e) t
  module Let_syntax : sig
    val return : 'a -> ('a, 'd, 'e) t
    include Infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t
    module Let_syntax : sig
      val return : 'a -> ('a, _, _) t
      val bind : ('a, 'd, 'e) t -> f:('a -> ('b, 'd, 'e) t) -> ('b, 'd, 'e) t
      val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
      val both : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> ('a * 'b, 'd, 'e) t
      module Open_on_rhs : sig end
    end
  end
end

module type S_without_syntax = sig
  type 'a t
  include Infix with type 'a t := 'a t
  module Monad_infix : Infix with type 'a t := 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
  val ignore_m : 'a t -> unit t
  val all : 'a t list -> 'a list t
  val all_unit : unit t list -> unit t
end

module type S = sig
  type 'a t
  include S_without_syntax with type 'a t := 'a t
  include Syntax with type 'a t := 'a t
end

module type S2 = sig
  type ('a, 'e) t
  include Infix2 with type ('a, 'e) t := ('a, 'e) t
  include Syntax2 with type ('a, 'e) t := ('a, 'e) t
  module Monad_infix : Infix2 with type ('a, 'e) t := ('a, 'e) t
  val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  val return : 'a -> ('a, _) t
  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
  val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
  val ignore_m : (_, 'e) t -> (unit, 'e) t
  val all : ('a, 'e) t list -> ('a list, 'e) t
  val all_unit : (unit, 'e) t list -> (unit, 'e) t
end

module type S3 = sig
  type ('a, 'd, 'e) t
  include Infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t
  include Syntax3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t
  module Monad_infix : Infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t
  val bind : ('a, 'd, 'e) t -> f:('a -> ('b, 'd, 'e) t) -> ('b, 'd, 'e) t
  val return : 'a -> ('a, _, _) t
  val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
  val join : (('a, 'd, 'e) t, 'd, 'e) t -> ('a, 'd, 'e) t
  val ignore_m : (_, 'd, 'e) t -> (unit, 'd, 'e) t
  val all : ('a, 'd, 'e) t list -> ('a list, 'd, 'e) t
  val all_unit : (unit, 'd, 'e) t list -> (unit, 'd, 'e) t
end

module Make (X: Basic) : S with type 'a t := 'a X.t = struct
  include Monad.Make (X)
  let (>>) m f = m >>= fun _ -> f

  module Monad_infix = struct
    include Monad_infix
    let (>>) = (>>)
  end

  module Let_syntax = struct
    include Let_syntax
    let (>>) = (>>)
  end
end

module Make2 (X: Basic2) : S2 with type ('a, 'e) t := ('a, 'e) X.t = struct
  include Monad.Make2 (X)
  let (>>) m f = m >>= fun _ -> f

  module Monad_infix = struct
    include Monad_infix
    let (>>) = (>>)
  end

  module Let_syntax = struct
    include Let_syntax
    let (>>) = (>>)
  end
end

module Make3 (X: Basic3) : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t = struct
  include Monad.Make3 (X)
  let (>>) m f = m >>= fun _ -> f

  module Monad_infix = struct
    include Monad_infix
    let (>>) = (>>)
  end

  module Let_syntax = struct
    include Let_syntax
    let (>>) = (>>)
  end
end
