open Core

let calendar = Map.of_alist_exn (module Int)
    [(1, ("data/day1.txt", [Day1.part1; Day1.part2]))
    ;(2, ("data/day2.txt", [Day2.part1; Day2.part2]))
    ;(3, ("data/day3.txt", [Day3.part1; Day3.part2]))
    ;(4, ("data/day4.txt", [Day4.part1; Day4.part2]))
    ;(5, ("data/day5.txt", [Day5.part1; Day5.part2]))
    ]

let () =
  let args = List.drop (Array.to_list @@ Sys.get_argv ()) 1 in
  let days =
    if List.is_empty args then
      (
        eprintf "Note: Assuming all days.\n%!";
        Map.keys calendar |> List.sort ~compare:Int.compare
      )
    else
      List.map args int_of_string
  in
  List.iter days ~f:(
    fun day ->
      match Map.find calendar day with
      | None ->
        eprintf "Error: No solution for day %d.\n%!" day
      | Some(input_file, soln_fns) ->
        printf "=== Day %d ===\n%!" day;
        List.iteri soln_fns
          (fun i soln_fn ->
            printf "* Part %d\n%!" (i + 1);
            In_channel.with_file input_file ~f:soln_fn)
  )
