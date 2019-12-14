open Core

type reaction =
  { output_amt : int
  ; requirements : (int * string) list
  }

type dependency = string Topological_sort.Edge.t

let sort_chems (chemicals: string list) (deps: dependency list): string list =
  match Topological_sort.sort (module String) chemicals deps with
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

let reaction_deps (reactions: reaction String.Map.t): dependency list =
  Map.fold reactions ~init:[] ~f:(
    fun ~key ~data deps ->
      List.map data.requirements (
        fun (_, chem) -> Topological_sort.Edge.{ from = chem; to_ = key }
      )
      @ deps
  )

let part1 file =
  let reactions, deps = input_reactions file in
  let chems = sort_chems [] deps in
  assert ([%equal: string] "FUEL" @@ List.hd_exn chems);
  assert ([%equal: string] "ORE" @@ List.last_exn chems);
  let chems = List.drop_last_exn chems in
  let reqs =
    let reqs = Map.set String.Map.empty "FUEL" 1 in
    List.fold chems ~init:reqs ~f:(
      fun reqs chem ->
        match Map.find reqs chem with
        | None -> reqs
        | Some need_amt ->
          let reqs = Map.remove reqs chem in
          let reaction = Map.find_exn reactions chem in
          let n_reactions =
            if need_amt % reaction.output_amt = 0
            then need_amt / reaction.output_amt
            else need_amt / reaction.output_amt + 1
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

let part2 file = ()
