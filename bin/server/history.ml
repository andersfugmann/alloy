open! Base
open! Stdio

let load path =
  match Stdlib.Sys.file_exists path with
  | true ->
    let content = In_channel.read_all path in
    (match Protocol.parse_json_string content with
     | Error _ -> []
     | Ok json ->
       match Protocol.history_of_yojson json with
       | Ok entries -> entries
       | Error _ -> [])
  | false -> []

let save path entries =
  let json = Protocol.history_to_yojson entries in
  let content = Yojson.Safe.pretty_to_string json in
  Out_channel.write_all path ~data:(content ^ "\n")

let record entries ~url ~title ~timestamp =
  let matching entry =
    String.equal entry.Protocol.url url && String.equal entry.Protocol.title title
  in
  let updated = { Protocol.url; title; timestamp } in
  match List.exists entries ~f:matching with
  | true -> List.map entries ~f:(fun entry ->
    match matching entry with
    | true -> updated
    | false -> entry)
  | false -> updated :: entries

let lookup entries ~query =
  let q = String.lowercase query in
  List.filter entries ~f:(fun entry ->
    String.is_substring (String.lowercase entry.Protocol.url) ~substring:q
    || String.is_substring (String.lowercase entry.Protocol.title) ~substring:q)
