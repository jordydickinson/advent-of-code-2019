open Core

type moon =
  { position : Zvec3.t 
  ; velocity : Zvec3.t
  }
[@@deriving equal, hash]

let simulate_step system =
  let apply_gravity m =
    { m with velocity =
        List.fold system ~init:m.velocity ~f:(
          fun mv m2 ->
            Zvec3.O.(mv - Zvec3.map2 m.position m2.position compare)
        )
    }
  in
  let apply_velocity m = 
    { m with position = Zvec3.O.(m.position + m.velocity) }
  in
  List.map system ~f:(
    fun moon -> moon |> apply_gravity |> apply_velocity
  )

let rec simulate system steps =
  if steps = 0 then system else
  simulate (simulate_step system) (steps - 1)

let potential_energy moon = Zvec3.norm moon.position
let kinetic_energy moon = Zvec3.norm moon.velocity

let system_energy moons =
  let energy moon = potential_energy moon * kinetic_energy moon in
  List.sum (module Int) moons ~f:energy

let input_moons file =
  In_channel.input_lines file
  |> List.map ~f:(
    fun line ->
      let position =
        String.filter line ~f:(
          fun c ->
            let baddies = Set.of_list (module Char) @@ String.to_list " xyz<>=" in
            not @@ Set.mem baddies c
        )
        |> String.split ~on:','
        |> List.foldi ~init:Zvec3.zero ~f:(
          fun i accum s ->
            Zvec3.add accum @@
            match i with
            | 0 -> Zvec3.O.(int_of_string s * Zvec3.xhat)
            | 1 -> Zvec3.O.(int_of_string s * Zvec3.yhat)
            | 2 -> Zvec3.O.(int_of_string s * Zvec3.zhat)
            | _ -> failwith "Invalid input"
        )
      in
      { position = position ; velocity = Zvec3.zero }
  )

let part1 file =
  let system = input_moons file in
  let system = simulate system 1000 in
  printf "Energy: %d\n" (system_energy system)

let part2 file =
  let system = input_moons file in
  let cycle_len system =
    let rec cycle_len' system' len =
      if List.equal [%equal: moon] system system'
      then len
      else cycle_len' (simulate_step system') (len + 1)
    in
    cycle_len' (simulate_step system) 1
  in
  let pseudosystem hat =
    List.map system (
      fun { position = r ; _ } ->
        { position = Zvec3.O.(r *+ hat * hat)
        ; velocity = Zvec3.zero
        }
    )
  in
  let rec gcd n m =
    if m = 0
    then n
    else gcd m (n%m)
  in
  let lcm n m =
    (n*m) / gcd n m
  in
  let x_cycle = cycle_len @@ pseudosystem Zvec3.xhat in
  let y_cycle = cycle_len @@ pseudosystem Zvec3.yhat in
  let z_cycle = cycle_len @@ pseudosystem Zvec3.zhat in
  printf "%d\n" @@ List.fold [x_cycle; y_cycle; z_cycle] ~init:1 ~f:lcm
