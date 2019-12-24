open Core

module X = struct
  type ('a, 's) t = 's -> 'a * 's
  let return x = fun s -> (x, s)
  let bind m ~f = fun r -> let (x, s) = m r in (f x) s
  let map = `Define_using_bind
end

include Monad_ext.Make2 (X)

type ('a, 's) t = ('a, 's) X.t

let get = fun s -> (s, s)
let set s = fun _ -> ((), s)
let run m s = m s
let eval m s = fst @@ run m s
