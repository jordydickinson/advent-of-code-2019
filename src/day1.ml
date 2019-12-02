open Core

let part1 file = string_of_int @@
  let calc_fuel fuel mass_s =
    fuel + (int_of_string mass_s) / 3 - 2
  in
  In_channel.fold_lines file ~init:0 ~f:calc_fuel

let part2 file = string_of_int @@
  let calc_fuel mass =
    let rec calc_fuel' fuel mass =
      let mass = mass / 3 - 2 in
      if mass <= 0 then fuel else
      let fuel = fuel + mass in
      calc_fuel' fuel mass
    in
    calc_fuel' 0 mass
  in
  In_channel.fold_lines file ~init:0
    ~f:(fun fuel mass_s -> fuel + calc_fuel (int_of_string mass_s))
