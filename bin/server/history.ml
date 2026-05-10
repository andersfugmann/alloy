open! Base
open! Stdio

let history_entries_of_yojson = function
  | `List items ->
    List.map items ~f:Protocol.history_entry_of_yojson
    |> Result.all
  | _ -> Error "expected list"

let history_entries_to_yojson entries =
  `List (List.map entries ~f:Protocol.history_entry_to_yojson)

let load path =
  match Stdlib.Sys.file_exists path with
  | true ->
    let content = In_channel.read_all path in
    (match Protocol.parse_json_string content with
     | Error _ -> Stdlib.Sys.remove path; []
     | Ok json ->
       match history_entries_of_yojson json with
       | Ok entries -> entries
       | Error _ -> Stdlib.Sys.remove path; [])
  | false -> []

let save path entries =
  let json = history_entries_to_yojson entries in
  let content = Yojson.Safe.pretty_to_string json in
  Out_channel.write_all path ~data:(content ^ "\n")

let record entries ~url ~title ~timestamp =
  let day = Float.to_int (timestamp /. 86400.) in
  let matching entry = String.equal entry.Protocol.url url in
  let update_visits visits =
    match visits with
    | d :: _ when Int.equal d day -> visits
    | _ -> day :: visits
  in
  match List.exists entries ~f:matching with
  | true -> List.map entries ~f:(fun entry ->
    match matching entry with
    | true -> { entry with Protocol.title; visits = update_visits entry.Protocol.visits }
    | false -> entry)
  | false -> { Protocol.url; title; visits = [ day ] } :: entries

let merge existing imported =
  let index =
    List.fold existing ~init:(Map.empty (module String)) ~f:(fun acc entry ->
      Map.set acc ~key:entry.Protocol.url ~data:entry)
  in
  let merged =
    List.fold imported ~init:index ~f:(fun acc entry ->
      Map.update acc entry.Protocol.url ~f:(function
        | None -> entry
        | Some existing_entry ->
          let days = Set.union
            (Set.of_list (module Int) existing_entry.Protocol.visits)
            (Set.of_list (module Int) entry.Protocol.visits)
          in
          let visits = Set.to_list days |> List.sort ~compare:(fun a b -> Int.compare b a) in
          let title =
            match String.is_empty entry.Protocol.title with
            | true -> existing_entry.Protocol.title
            | false -> entry.Protocol.title
          in
          { Protocol.url = entry.url; title; visits }))
  in
  Map.data merged

let lookup entries ~query ~(scope : Protocol.search_scope) ~max_results ~max_age_days =
  let terms =
    query
    |> String.lowercase
    |> String.split_on_chars ~on:[' '; '\t']
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  let term_matches entry term =
    let in_url = String.is_substring (String.lowercase entry.Protocol.url) ~substring:term in
    let in_title = String.is_substring (String.lowercase entry.Protocol.title) ~substring:term in
    match scope with
    | Url -> in_url
    | Title -> in_title
    | Both -> in_url || in_title
  in
  let match_count entry =
    List.count terms ~f:(term_matches entry)
  in
  let today = Float.to_int (Unix.gettimeofday () /. 86400.) in
  let within_age entry =
    match max_age_days with
    | None -> true
    | Some max_days ->
      match List.hd entry.Protocol.visits with
      | None -> false
      | Some most_recent -> today - most_recent <= max_days
  in
  let visit_score visits =
    List.fold visits ~init:0.0 ~f:(fun acc day ->
      let age = today - day in
      acc +. 1.0 /. (1.0 +. Float.of_int age))
  in
  let score matches entry =
    Float.of_int matches *. 1000.0 +. visit_score entry.Protocol.visits
  in
  entries
  |> List.filter ~f:within_age
  |> List.filter_map ~f:(fun entry ->
    match match_count entry with
    | 0 -> None
    | n -> Some (score n entry, Protocol.{ entry; matches = n }))
  |> List.sort ~compare:(fun (s1, _) (s2, _) -> Float.compare s2 s1)
  |> (fun results ->
    match List.length results > max_results with
    | true -> List.take results max_results
    | false -> results)
  |> List.map ~f:snd
