open Core

let calendar = Map.of_alist_exn (module Int)
    [(1, ("data/day1.txt", [Day1.part1; Day1.part2]))
    ;(2, ("data/day2.txt", [Day2.part1; Day2.part2]))
    ;(3, ("data/day3.txt", [Day3.part1; Day3.part2]))
    ;(4, ("data/day4.txt", [Day4.part1; Day4.part2]))
    ;(5, ("data/day5.txt", [Day5.part1; Day5.part2]))
    ;(6, ("data/day6.txt", [Day6.part1; Day6.part2]))
    ;(7, ("data/day7.txt", [Day7.part1; Day7.part2]))
    ;(8, ("data/day8.txt", [Day8.part1; Day8.part2]))
    ;(9, ("data/day9.txt", [Day9.part1; Day9.part2]))
    ;(10, ("data/day10.txt", [Day10.part1; Day10.part2]))
    ;(11, ("data/day11.txt", [Day11.part1; Day11.part2]))
    ;(12, ("data/day12.txt", [Day12.part1; Day12.part2]))
    ;(13, ("data/day13.txt", [Day13.part1; Day13.part2]))
    ;(14, ("data/day14.txt", [Day14.part1; Day14.part2]))
    ;(15, ("data/day15.txt", [Day15.part1]))
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
