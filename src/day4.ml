open Core

let digits_of_pass pass =
  let rec digits_of_pass' accum pass =
    if pass = 0 then accum else
    let digit = pass % 10 in
    digits_of_pass' (digit :: accum) (pass/10)
  in
  digits_of_pass' [] pass

let is_valid pass =
  let rec is_valid' prev digits has_repeat =
    match digits with
    | [] -> has_repeat
    | digit :: digits when digit < prev -> false
    | digit :: digits when digit = prev -> is_valid' digit digits true
    | digit :: digits -> is_valid' digit digits has_repeat
  in
  let digits = digits_of_pass pass in
  is_valid' (List.hd_exn digits) (List.tl_exn digits) false

let is_valid2 pass =
  let rec is_valid' prev digits consecs has_repeat =
    match digits with
    | [] ->
      if consecs = 2
      then true
      else has_repeat
    | digit :: digits when digit < prev -> false
    | digit :: digits when digit = prev ->
      is_valid' digit digits (consecs + 1) has_repeat
    | digit :: digits -> (* digit <> prev *)
      if consecs = 2
      then is_valid' digit digits 1 true
      else is_valid' digit digits 1 has_repeat
  in
  let digits = digits_of_pass pass in
  is_valid' (List.hd_exn digits) (List.tl_exn digits) 1 false

let bounds_of_file file =
  let bounds =
    In_channel.input_all file
    |> String.split ~on:'-'
    |> List.map ~f:(fun s -> String.strip s |> int_of_string)
  in
  List.hd_exn bounds, List.hd_exn @@ List.tl_exn bounds

let part1 file =
  let lower, upper = bounds_of_file file in
  Sequence.range ~stop:`inclusive lower upper
  |> Sequence.filter ~f:is_valid
  |> Sequence.length
  |> printf "%d\n"

let part2 file =
  let lower, upper = bounds_of_file file in
  Sequence.range ~stop:`inclusive lower upper
  |> Sequence.filter ~f:is_valid2
  |> Sequence.length
  |> printf "%d\n"
