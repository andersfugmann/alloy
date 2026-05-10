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

let delete entries ~urls =
  let url_set = Set.of_list (module String) urls in
  List.filter entries ~f:(fun entry ->
    not (Set.mem url_set entry.Protocol.url))

let lookup entries ~query ~(scope : Protocol.search_scope) ~max_results ~max_age_days ~today =
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

let print_entries entries =
  List.iter entries ~f:(fun e ->
    printf "  %s | %s | visits: [%s]\n"
      e.Protocol.url e.Protocol.title
      (List.map e.Protocol.visits ~f:Int.to_string |> String.concat ~sep:", "))

let print_results results =
  List.iter results ~f:(fun (r : Protocol.lookup_result) ->
    printf "  %s | %s | matches: %d | visits: [%s]\n"
      r.entry.url r.entry.title r.matches
      (List.map r.entry.visits ~f:Int.to_string |> String.concat ~sep:", "))

let%expect_test "record: new url creates entry" =
  let entries = record [] ~url:"https://a.com" ~title:"A" ~timestamp:86400.0 in
  print_entries entries;
  [%expect {| https://a.com | A | visits: [1] |}]

let%expect_test "record: same url same day is idempotent" =
  let entries = record [] ~url:"https://a.com" ~title:"A" ~timestamp:86400.0 in
  let entries = record entries ~url:"https://a.com" ~title:"A" ~timestamp:86400.5 in
  print_entries entries;
  [%expect {| https://a.com | A | visits: [1] |}]

let%expect_test "record: same url different day adds visit" =
  let entries = record [] ~url:"https://a.com" ~title:"A" ~timestamp:86400.0 in
  let entries = record entries ~url:"https://a.com" ~title:"A" ~timestamp:172800.0 in
  print_entries entries;
  [%expect {| https://a.com | A | visits: [2, 1] |}]

let%expect_test "record: updates title" =
  let entries = record [] ~url:"https://a.com" ~title:"Old" ~timestamp:86400.0 in
  let entries = record entries ~url:"https://a.com" ~title:"New" ~timestamp:172800.0 in
  print_entries entries;
  [%expect {| https://a.com | New | visits: [2, 1] |}]

let%expect_test "record: multiple urls" =
  let entries = record [] ~url:"https://a.com" ~title:"A" ~timestamp:86400.0 in
  let entries = record entries ~url:"https://b.com" ~title:"B" ~timestamp:172800.0 in
  print_entries entries;
  [%expect {|
    https://b.com | B | visits: [2]
    https://a.com | A | visits: [1] |}]

let%expect_test "merge: disjoint urls" =
  let existing = [{ Protocol.url = "https://a.com"; title = "A"; visits = [2; 1] }] in
  let imported = [{ Protocol.url = "https://b.com"; title = "B"; visits = [3] }] in
  let result = merge existing imported in
  print_entries result;
  [%expect {|
    https://a.com | A | visits: [2, 1]
    https://b.com | B | visits: [3] |}]

let%expect_test "merge: overlapping urls union visits" =
  let existing = [{ Protocol.url = "https://a.com"; title = "A"; visits = [3; 1] }] in
  let imported = [{ Protocol.url = "https://a.com"; title = "A2"; visits = [3; 2] }] in
  let result = merge existing imported in
  print_entries result;
  [%expect {| https://a.com | A2 | visits: [3, 2, 1] |}]

let%expect_test "merge: empty import title preserves existing" =
  let existing = [{ Protocol.url = "https://a.com"; title = "Good Title"; visits = [1] }] in
  let imported = [{ Protocol.url = "https://a.com"; title = ""; visits = [2] }] in
  let result = merge existing imported in
  print_entries result;
  [%expect {| https://a.com | Good Title | visits: [2, 1] |}]

let%expect_test "merge: idempotent re-merge" =
  let entries = [
    { Protocol.url = "https://a.com"; title = "A"; visits = [3; 2; 1] };
    { Protocol.url = "https://b.com"; title = "B"; visits = [2] };
  ] in
  let result = merge entries entries in
  print_entries result;
  [%expect {|
    https://a.com | A | visits: [3, 2, 1]
    https://b.com | B | visits: [2] |}]

let%expect_test "lookup: matches by url scope" =
  let entries = [
    { Protocol.url = "https://example.com"; title = "Foo"; visits = [10] };
    { Protocol.url = "https://other.com"; title = "Example Page"; visits = [10] };
  ] in
  let results = lookup entries ~query:"example" ~scope:Url ~max_results:10 ~max_age_days:None ~today:10 in
  print_results results;
  [%expect {| https://example.com | Foo | matches: 1 | visits: [10] |}]

let%expect_test "lookup: matches by title scope" =
  let entries = [
    { Protocol.url = "https://example.com"; title = "Foo"; visits = [10] };
    { Protocol.url = "https://other.com"; title = "Example Page"; visits = [10] };
  ] in
  let results = lookup entries ~query:"example" ~scope:Title ~max_results:10 ~max_age_days:None ~today:10 in
  print_results results;
  [%expect {| https://other.com | Example Page | matches: 1 | visits: [10] |}]

let%expect_test "lookup: matches by both scope" =
  let entries = [
    { Protocol.url = "https://example.com"; title = "Foo"; visits = [10] };
    { Protocol.url = "https://other.com"; title = "Example Page"; visits = [10] };
  ] in
  let results = lookup entries ~query:"example" ~scope:Both ~max_results:10 ~max_age_days:None ~today:10 in
  print_results results;
  [%expect {|
    https://example.com | Foo | matches: 1 | visits: [10]
    https://other.com | Example Page | matches: 1 | visits: [10] |}]

let%expect_test "lookup: scoring favors more matches" =
  let entries = [
    { Protocol.url = "https://foo.com/bar"; title = "Foo Bar"; visits = [10] };
    { Protocol.url = "https://foo.com"; title = "Only Foo"; visits = [10] };
  ] in
  let results = lookup entries ~query:"foo bar" ~scope:Both ~max_results:10 ~max_age_days:None ~today:10 in
  print_results results;
  [%expect {|
    https://foo.com/bar | Foo Bar | matches: 2 | visits: [10]
    https://foo.com | Only Foo | matches: 1 | visits: [10] |}]

let%expect_test "lookup: scoring favors recent visits" =
  let entries = [
    { Protocol.url = "https://old.com"; title = "Test Old"; visits = [5] };
    { Protocol.url = "https://new.com"; title = "Test New"; visits = [10] };
  ] in
  let results = lookup entries ~query:"test" ~scope:Both ~max_results:10 ~max_age_days:None ~today:10 in
  print_results results;
  [%expect {|
    https://new.com | Test New | matches: 1 | visits: [10]
    https://old.com | Test Old | matches: 1 | visits: [5] |}]

let%expect_test "lookup: max_results limits output" =
  let entries = [
    { Protocol.url = "https://a.com"; title = "Test A"; visits = [10] };
    { Protocol.url = "https://b.com"; title = "Test B"; visits = [9] };
    { Protocol.url = "https://c.com"; title = "Test C"; visits = [8] };
  ] in
  let results = lookup entries ~query:"test" ~scope:Both ~max_results:2 ~max_age_days:None ~today:10 in
  print_results results;
  [%expect {|
    https://a.com | Test A | matches: 1 | visits: [10]
    https://b.com | Test B | matches: 1 | visits: [9] |}]

let%expect_test "lookup: max_age_days filters old entries" =
  let entries = [
    { Protocol.url = "https://old.com"; title = "Test Old"; visits = [1] };
    { Protocol.url = "https://new.com"; title = "Test New"; visits = [9] };
  ] in
  let results = lookup entries ~query:"test" ~scope:Both ~max_results:10 ~max_age_days:(Some 3) ~today:10 in
  print_results results;
  [%expect {| https://new.com | Test New | matches: 1 | visits: [9] |}]

let%expect_test "lookup: no matches returns empty" =
  let entries = [
    { Protocol.url = "https://a.com"; title = "Alpha"; visits = [10] };
  ] in
  let results = lookup entries ~query:"zzz" ~scope:Both ~max_results:10 ~max_age_days:None ~today:10 in
  print_results results;
  [%expect {| |}]

let%expect_test "lookup: empty query returns empty" =
  let entries = [
    { Protocol.url = "https://a.com"; title = "Alpha"; visits = [10] };
  ] in
  let results = lookup entries ~query:"" ~scope:Both ~max_results:10 ~max_age_days:None ~today:10 in
  print_results results;
  [%expect {| |}]

let%expect_test "lookup: multi-word query" =
  let entries = [
    { Protocol.url = "https://github.com/ocaml"; title = "OCaml on GitHub"; visits = [10] };
    { Protocol.url = "https://github.com/rust"; title = "Rust on GitHub"; visits = [10] };
  ] in
  let results = lookup entries ~query:"github ocaml" ~scope:Both ~max_results:10 ~max_age_days:None ~today:10 in
  print_results results;
  [%expect {|
    https://github.com/ocaml | OCaml on GitHub | matches: 2 | visits: [10]
    https://github.com/rust | Rust on GitHub | matches: 1 | visits: [10] |}]

let%expect_test "delete: removes matching urls" =
  let entries = [
    { Protocol.url = "https://a.com"; title = "A"; visits = [3; 1] };
    { Protocol.url = "https://b.com"; title = "B"; visits = [2] };
    { Protocol.url = "https://c.com"; title = "C"; visits = [1] };
  ] in
  let result = delete entries ~urls:["https://b.com"] in
  print_entries result;
  [%expect {|
    https://a.com | A | visits: [3, 1]
    https://c.com | C | visits: [1] |}]

let%expect_test "delete: removes multiple urls" =
  let entries = [
    { Protocol.url = "https://a.com"; title = "A"; visits = [3] };
    { Protocol.url = "https://b.com"; title = "B"; visits = [2] };
    { Protocol.url = "https://c.com"; title = "C"; visits = [1] };
  ] in
  let result = delete entries ~urls:["https://a.com"; "https://c.com"] in
  print_entries result;
  [%expect {| https://b.com | B | visits: [2] |}]

let%expect_test "delete: non-existent url is no-op" =
  let entries = [
    { Protocol.url = "https://a.com"; title = "A"; visits = [1] };
  ] in
  let result = delete entries ~urls:["https://z.com"] in
  print_entries result;
  [%expect {| https://a.com | A | visits: [1] |}]

let%expect_test "delete: empty url list is no-op" =
  let entries = [
    { Protocol.url = "https://a.com"; title = "A"; visits = [1] };
  ] in
  let result = delete entries ~urls:[] in
  print_entries result;
  [%expect {| https://a.com | A | visits: [1] |}]
