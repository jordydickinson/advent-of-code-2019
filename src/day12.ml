open Core

type moon =
  { position : Zvec.t 
  ; velocity : Zvec.t
  }

let time_step_inplace moons =
  let apply_gravity (m1, m2) =
      let r1, v1, r2, v2 =
        m1.position, m1.velocity,
        m2.position, m2.velocity
      in
      for i = 0 to 2 do
        if r1.(i) > r2.(i) then
          v1.(i) <- v1.(i) - 1
        else if r1.(i) < r2.(i) then
          v1.(i) <- v1.(i) + 1
      done
  in
  let apply_velocity m = 
    Zvec.add_inplace m.position m.velocity
  in
  List.cartesian_product moons moons
  |> List.iter ~f:apply_gravity;
  List.iter moons ~f:apply_velocity

let energy moons =
  let potential1 moon = Zvec.norm moon.position in
  let kinetic1 moon = Zvec.norm moon.velocity in
  let energy1 moon = potential1 moon * kinetic1 moon in
  List.sum (module Int) moons ~f:energy1

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
        |> Array.of_list_map ~f:Int.of_string
      in
      { position = position ; velocity = Zvec.zero 3 }
  )

let print_moons moons =
  List.iter moons ~f:(
    fun m ->
      let pos_s = 
        Array.map m.position ~f:string_of_int
        |> String.concat_array ~sep:", "
      in
      let vel_s =
        Array.map m.velocity ~f:string_of_int
        |> String.concat_array ~sep:", "
      in
      printf "pos=<%s>, vel=<%s>\n" pos_s vel_s
  )

let part1 file =
  let moons = input_moons file in
  for i = 1 to 1000 do
    time_step_inplace moons
  done;
  print_moons moons;
  printf "Energy: %d\n" (energy moons);
