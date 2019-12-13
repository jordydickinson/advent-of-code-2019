open Core

type t =
  { mutable memory : int array
  ; mutable relative_base : int
  }

let copy machine =
  { memory = Array.copy machine.memory
  ; relative_base = machine.relative_base
  }

let ensure_addr machine addr =
  let memlen = Array.length machine.memory in
  if addr >= memlen then begin
    machine.memory <-
      Array.append machine.memory (Array.create memlen 0)
  end

let get machine addr =
  ensure_addr machine addr;
  machine.memory.(addr)

let get_relative machine addr =
  let addr = machine.relative_base + addr in
  get machine addr

let set machine addr x =
  ensure_addr machine addr;
  machine.memory.(addr) <- x

let set_relative machine addr x =
  let addr = machine.relative_base + addr in
  set machine addr x

type arg =
  | Value of int
  | Pointer of int
  | Relative of int

let value machine arg =
  match arg with
  | Value v -> v
  | Pointer p -> get machine p
  | Relative r -> get_relative machine r

let write_exn machine out x =
  match out with
  | Value _ -> failwith "Tried to write to immediate value"
  | Pointer p -> set machine p x
  | Relative r -> set_relative machine r x

type op =
  | ADD of arg * arg * arg
  | MUL of arg * arg * arg
  | INPUT of arg
  | OUTPUT of arg
  | JNZ of arg * arg
  | JZ of arg * arg
  | LT of arg * arg * arg
  | EQ of arg * arg * arg
  | ADJB of arg
  | HALT

let read_op machine pos =
  let mem = machine.memory in
  let opcode = mem.(pos) % 100 in
  let mode i = mem.(pos) / (100 * Int.pow 10 i) % 10 in
  let arg i =
    let value = mem.(pos + 1 + i) in
    match mode i with
    | 0 -> Pointer value
    | 1 -> Value value
    | 2 -> Relative value
    | m -> failwithf "Invalid parameter mode %d at address %d" m i ()
  in
  match opcode with
  | 1 -> ADD (arg 0, arg 1, arg 2)
  | 2 -> MUL (arg 0, arg 1, arg 2)
  | 3 -> INPUT (arg 0)
  | 4 -> OUTPUT (arg 0)
  | 5 -> JNZ (arg 0, arg 1)
  | 6 -> JZ (arg 0, arg 1)
  | 7 -> LT (arg 0, arg 1, arg 2)
  | 8 -> EQ (arg 0, arg 1, arg 2)
  | 9 -> ADJB (arg 0)
  | 99 -> HALT
  | _ -> failwithf "Invalid opcode %d at address %d" opcode pos ()

type state =
  | Receiving of (int -> state)
  | Sending of int * (unit -> state)
  | Halted

let exec machine =
  let machine = copy machine in
  let value = value machine in
  let write_exn = write_exn machine in
  let read_op = read_op machine in
  let rec jmp ip =
    let op = read_op ip in
    match op with
    | ADD (in1, in2, out) ->
      write_exn out (value in1 + value in2);
      jmp (ip + 4)
    | MUL (in1, in2, out) ->
      write_exn out (value in1 * value in2);
      jmp (ip + 4)
    | INPUT out ->
      Receiving (fun in1 -> write_exn out in1; jmp (ip + 2))
    | OUTPUT in1 ->
      Sending (value in1, fun () -> jmp (ip + 2))
    | JNZ (test, dest) ->
      if value test <> 0
      then jmp (value dest)
      else jmp (ip + 3)
    | JZ (test, dest) ->
      if value test = 0
      then jmp (value dest)
      else jmp (ip + 3)
    | LT (in1, in2, out) ->
      if value in1 < value in2
      then write_exn out 1
      else write_exn out 0;
      jmp (ip + 4)
    | EQ (in1, in2, out) ->
      if value in1 = value in2
      then write_exn out 1
      else write_exn out 0;
      jmp (ip + 4)
    | ADJB offset ->
      machine.relative_base <- machine.relative_base + value offset;
      jmp (ip + 2)
    | HALT -> Halted
  in
  jmp 0

let load file =
  let mem =
    In_channel.input_all file
    |> String.strip
    |> String.split ~on:','
    |> Array.of_list_map ~f:int_of_string
  in
  { memory = mem; relative_base = 0 }

let send_exn s ~input =
  match s with
  | Receiving resume_with -> resume_with input
  | _ -> failwith "Unexpected machine state."

let recv s =
  match s with
  | Sending (value, resume) -> resume (), Some value
  | _ -> s, None

let recv_many s =
  let rec recv_many' accum s =
    match recv s with
    | s, None -> s, List.rev accum
    | s, Some value -> recv_many' (value :: accum) s
  in
  recv_many' [] s

let recv_exn s =
  match s with
  | Sending (value, resume) -> resume (), value
  | _ -> failwith "Unexpected machine state."

let is_halted s =
  match s with
  | Halted -> true
  | _ -> false

let expect_halted_exn s =
  match s with
  | Halted -> ()
  | _ -> failwith "Unexpected machine state."

let returns_exn s =
  let s, value = recv_exn s in
  expect_halted_exn s;
  value

let collect_exn s =
  let rec collect_exn' accum s =
    if is_halted s then List.rev accum else
    let s, value = recv_exn s in
    collect_exn' (value :: accum) s
  in
  collect_exn' [] s
