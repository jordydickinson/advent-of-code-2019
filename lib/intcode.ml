open Core

type arg =
  | Value of int
  | Pointer of int

let value mem arg =
  match arg with
  | Value v -> v
  | Pointer p -> mem.(p)

let write_exn mem out x =
  match out with
  | Value _ -> failwith "Tried to write to immediate value"
  | Pointer p -> mem.(p) <- x

type op =
  | ADD of arg * arg * arg
  | MUL of arg * arg * arg
  | INPUT of arg
  | OUTPUT of arg
  | JNZ of arg * arg
  | JZ of arg * arg
  | LT of arg * arg * arg
  | EQ of arg * arg * arg
  | HALT

let read_op mem pos =
  let opcode = mem.(pos) % 100 in
  let mode i = mem.(pos) / (100 * Int.pow 10 i) % 10 in
  let arg i =
    if mode i = 1
    then Value mem.(pos + 1 + i)
    else Pointer mem.(pos + 1 + i)
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
  | 99 -> HALT
  | _ -> failwithf "Invalid opcode %d at address %d" opcode pos ()

type state =
  | Receiving of (int -> state)
  | Sending of int * (unit -> state)
  | Halted

let exec mem =
  let mem = Array.copy mem in
  let value = value mem in
  let write_exn = write_exn mem in
  let read_op = read_op mem in
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
    | HALT -> Halted
  in
  jmp 0

let load file =
  In_channel.input_all file
  |> String.strip
  |> String.split ~on:','
  |> Array.of_list_map ~f:int_of_string

let send_exn s ~input =
  match s with
  | Receiving resume_with -> resume_with input
  | _ -> failwith "Unexpected machine state."

let recv_exn s =
  match s with
  | Sending (value, resume) -> resume (), value
  | _ -> failwith "Unexpected machine state."

let expect_halted_exn s =
  match s with
  | Halted -> ()
  | _ -> failwith "Unexpected machine state."

let returns_exn s =
  let s, value = recv_exn s in
  expect_halted_exn s;
  value
