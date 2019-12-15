open Core

type reaction =
  { output_amt : int
  ; requirements : (int * string) list
  }
[@@deriving compare, hash]

type dependency = string Topological_sort.Edge.t

let sort_chems (deps: dependency list): string list =
  match Topological_sort.sort (module String) [] deps with
  | Ok sorted -> sorted
  | Error e -> Error.raise e

let input_reactions file : reaction String.Map.t * dependency list =
  In_channel.fold_lines file ~init:(String.Map.empty, []) ~f:(
    fun (reactions, deps) line ->
      let spliti = String.substr_index line " => " |> Option.value_exn in
      let prereqs =
        String.prefix line (spliti + 1)
        |> String.split ~on:','
        |> List.map ~f:(
          fun prereq_s ->
            match String.strip prereq_s |> String.split ~on:' ' with
            | [amt_s; chem] -> int_of_string amt_s, chem
            | _ -> failwithf "Bad prereq format: %s" prereq_s ()
        )
      in
      let out_amt, out_chem =
        let out_s = String.drop_prefix line (spliti + 4) in
        match out_s |> String.strip |> String.split ~on: ' ' with
        | [amt_s; chem] -> int_of_string amt_s, chem
        | _ -> failwithf "Bad output format: %s" out_s ()
      in
      let reactions =
        let reaction = { output_amt = out_amt; requirements = prereqs } in
        Map.add_exn reactions ~key:out_chem ~data:reaction
      in
      let deps = List.fold prereqs ~init:deps ~f:(
        fun deps (_, chem) ->
          let dep = Topological_sort.Edge.{ from = out_chem; to_ = chem } in
          dep :: deps
      )
      in
      reactions, deps
  )

let produce reactions deps =
  let open Result.Let_syntax in
  let chems = sort_chems deps in
  let produce' resources chem =
    let product_reqs = String.Map.of_alist_exn [chem, 1] in
    List.fold_result chems ~init:(product_reqs, resources)
      ~f:(fun (product_reqs, resources) chem ->
        match Map.find product_reqs chem with
        | None -> Ok (product_reqs, resources)
        | Some chem_amt ->
          (* Remove the chemical from the production requirements. *)
          let product_reqs = Map.remove product_reqs chem in
          (* We may already have some resources to meet this requirement. *)
          let have_amt = Map.find resources chem |> Option.value ~default:0 in
          if have_amt >= chem_amt then
            (* We're in luck, we can just deduct this from our resources and carry on. *)
            let resources = Map.set resources chem (have_amt - chem_amt) in
            Ok (product_reqs, resources)
          else
            (* We may still be able to use some of it towards production. *)
            let chem_amt = chem_amt - have_amt in
            let%bind reaction = Map.find_or_error reactions chem in
            (* Determine how many reactions we'll need to do to meet the requirement. *)
            let n_reactions =
              if chem_amt % reaction.output_amt = 0
              then chem_amt / reaction.output_amt
              else chem_amt / reaction.output_amt + 1 in
            (* We might produce some extra byproduct. *)
            let byproduct_amt = n_reactions * reaction.output_amt - chem_amt in
            let resources = Map.set resources chem byproduct_amt in
            (* Update production requirements with the requirements of the reactions. *)
            let product_reqs = List.fold reaction.requirements ~init:product_reqs ~f:(
              fun product_reqs (req_amt, req_chem) -> Map.update product_reqs req_chem
                (Option.value_map ~default:(n_reactions * req_amt) ~f:((+) (n_reactions * req_amt)))) in
            Ok (product_reqs, resources))
    |> Result.map ~f:snd in
  produce'

let part1 file =
  let reactions, deps = input_reactions file in
  let chems = sort_chems deps in
  assert ([%equal: string] "FUEL" @@ List.hd_exn chems);
  assert ([%equal: string] "ORE" @@ List.last_exn chems);
  let chems = List.drop_last_exn chems in
  let reqs =
    let reqs = Map.set String.Map.empty "FUEL" 1 in
    List.fold chems ~init:reqs ~f:(
      fun reqs chem ->
        match Map.find reqs chem with
        | None -> reqs
        | Some amt ->
          let reqs = Map.remove reqs chem in
          let reaction = Map.find_exn reactions chem in
          let n_reactions =
            if amt % reaction.output_amt = 0
            then amt / reaction.output_amt
            else amt / reaction.output_amt + 1
          in
          List.fold reaction.requirements ~init:reqs ~f:(
            fun reqs (req_amt, req_chem) ->
              Map.update reqs req_chem ~f:(function
                | None -> n_reactions * req_amt
                | Some amt -> amt + n_reactions * req_amt
              )
          )
    )
  in
  assert (Map.length reqs = 1);
  Map.find_exn reqs "ORE"
  |> printf "%d\n"

let part2 file =
  let reactions, deps = input_reactions file in
  let resources = String.Map.of_alist_exn ["ORE", 1_000_000_000_000] in
  let produce = produce reactions deps in
  let rec produce_fuel fuel resources =
    match produce resources "FUEL" with
    | Error _ -> fuel
    | Ok resources -> produce_fuel (fuel + 1) resources
  in
  printf "%d\n" (produce_fuel 0 resources)
