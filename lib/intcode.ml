open Core

module T = struct
  type ('x, 's) t = 's -> 'x * 's

  let return x = fun s -> (x, s)
  let bind m ~f = fun r -> let (x, s) = m r in (f x) s
  let map = `Define_using_bind
end

include Monad.Make2 (T)
open Let_syntax

type machine =
  { memory : int Int.Map.t
  ; iptr : int
  ; baseptr : int
  ; inputs : int list
  }

type error =
  | Blocking
  | Halted

type 'a t = ('a, machine) T.t

type param_mode =
  | Position
  | Immediate
  | BaseRelative

let get () = fun s -> (s, s)
let set s () = fun _ -> ((), s)

let check_addr addr =
  if addr < 0 then failwith "Negative addr"

let getiptr () =
  let%map s = get () in
  s.iptr

let jump addr () =
  let%bind s = get () in
  set { s with iptr = addr } ()

let getbase () =
  let%map s = get () in
  s.baseptr

let setbase baseptr () =
  let%bind s = get () in
  set { s with baseptr = baseptr } ()

let getmem addr () =
  check_addr addr;
  let%map s = get () in
  Map.find s.memory addr
    |> Option.value ~default:0

let getmem_relative offset () =
  let%bind baseptr = getbase () in
  getmem (baseptr + offset) ()

let setmem addr x () =
  check_addr addr;
  let%bind s = get () in
  set { s with memory = Map.set s.memory addr x } ()

let setmem_relative offset x () =
  let%bind baseptr = getbase () in
  setmem (baseptr + offset) x ()

let skip offset () =
  let%bind iptr = getiptr () in
  jump (iptr + offset) ()

let getmode i =
  let%bind iptr = getiptr () in
  let%map opcode = getmem iptr () in
  match opcode / (100 * Int.pow 10 i) % 10 with
  | 0 -> Position
  | 1 -> Immediate
  | 2 -> BaseRelative
  | m -> failwithf "Invalid parameter mode %d at address %d" m iptr ()

let arg i =
  let%bind iptr = getiptr () in
  match%bind getmode i with
  | Position ->
    let%bind p = getmem (iptr + i + 1) () in getmem p ()
  | Immediate -> getmem (iptr + i + 1) ()
  | BaseRelative ->
    let%bind p = getmem (iptr + i + 1) () in getmem_relative p ()

let ret i x () =
  let%bind iptr = getiptr () in
  match%bind getmode i with
  | Position ->
    let%bind p = getmem (iptr + i + 1) () in
    setmem p x () >>=
    skip (i + 2)
  | Immediate -> failwith "Attempted to write to immediate value"
  | BaseRelative ->
    let%bind p = getmem (iptr + i + 1) () in
    setmem_relative p x () >>=
    skip (i + 2)

let add in1 in2 ret =
  let%bind lhs = in1 in
  let%bind rhs = in2 in
  ret (lhs + rhs) ()

let mul in1 in2 ret =
  let%bind lhs = in1 in
  let%bind rhs = in2 in
  ret (lhs * rhs) ()

let input ret =
  let%bind s = get () in
  match s.inputs with
  | [] -> return (Error Blocking)
  | x :: xs ->
  set { s with inputs = xs } () >>=
  ret x >>= fun () ->
  return (Ok ())

let output in1 =
  let%bind out = in1 in
  skip 2 () >>= fun () ->
  return @@ Ok out

let jump_nonzero in1 ptr =
  let%bind test = in1 in
  let%bind addr = ptr in
  if test <> 0
  then jump addr ()
  else skip 3 ()

let jump_zero in1 ptr =
  let%bind test = in1 in
  let%bind addr = ptr in
  if test = 0
  then jump addr ()
  else skip 3 ()

let lt in1 in2 ret =
  let%bind lhs = in1 in
  let%bind rhs = in2 in
  if lhs < rhs
  then ret 1 ()
  else ret 0 ()

let eq in1 in2 ret =
  let%bind lhs = in1 in
  let%bind rhs = in2 in
  if lhs = rhs
  then ret 1 ()
  else ret 0 ()

let adjust_baseptr in1 =
  let%bind offset = in1 in
  let%bind baseptr = getbase () in
  setbase (baseptr + offset) () >>=
  skip 2

let halt () = return (Error Halted)

let load file =
  let mem =
    In_channel.input_all file
    |> String.strip
    |> String.split ~on:','
    |> List.foldi ~init:Int.Map.empty
      ~f:(fun i mem data_s -> Map.set mem i (int_of_string data_s))
  in
  { memory = mem; iptr = 0; baseptr = 0; inputs = [] }

let run m s = m s
let eval m s = fst @@ run m s

let write x () =
  let%bind s = get () in
  set { s with inputs = s.inputs @ [x] } ()

let write_all xs () =
  let%bind s = get () in
  set { s with inputs = s.inputs @ xs } ()

let write_char c () =
  write (int_of_char c) ()

let write_string s () =
  let xs = String.to_list s
    |> List.map ~f:int_of_char in
  write_all xs ()

let rec read () =
  let%bind iptr = getiptr () in
  let%bind opcode = getmem iptr () in
  match opcode % 100 with
  | 1 -> add (arg 0) (arg 1) (ret 2) >>= read
  | 2 -> mul (arg 0) (arg 1) (ret 2) >>= read
  | 3 -> input (ret 0) >>= (function | Ok () -> read () | Error e -> return (Error e))
  | 4 -> output (arg 0)
  | 5 -> jump_nonzero (arg 0) (arg 1) >>= read
  | 6 -> jump_zero (arg 0) (arg 1) >>= read
  | 7 -> lt (arg 0) (arg 1) (ret 2) >>= read
  | 8 -> eq (arg 0) (arg 1) (ret 2) >>= read
  | 9 -> adjust_baseptr (arg 0) >>= read
  | 99 -> halt ()
  | _ -> failwithf "Invalid opcode %d at address %d" opcode iptr ()

let peek () =
  let%bind s = get () in
  let%bind x = read () in
  set s () >>= fun () ->
  return x

let read_char () =
  read () >>| Result.map ~f:char_of_int

let peek_char () =
  peek () >>| Result.map ~f:char_of_int

let fold_read ~init ~f () =
  let rec fold_m' accum m =
    match%bind m with
    | Error _ -> return accum
    | Ok x ->
      let%bind accum = f accum x in
      fold_m' accum (read ())
  in
  let%bind init = init in
  fold_m' init (read ())

let collect ~f () =
  let f accum x = return (f x :: accum) in
  let%map xs_rev = fold_read ~init:(return []) ~f () in
  List.rev xs_rev

let read_string () =
  let%map chars = collect ~f:char_of_int () in
  String.of_char_list chars

let is_halted () =
  match%map peek () with
  | Error Halted -> true
  | _ -> false
